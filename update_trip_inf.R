# This file updates trip_infeasibility plot

# handline commandline args...
# rem: with TRUE option below, #args[1] is the "--args" switch; skip it.
# args <- commandArgs(TRUE)
# print(args)
a_id <- 27 #args[2]

# setwd(basedir)

# utility for checking is something is already installed, then loading.
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[, 1])) {
        # print(p)
        install.packages(p, dep = TRUE, repos = "http://cran.us.r-project.org")
    }
    require(p, character.only = TRUE)
}

usePackage("dplyr")
usePackage("RPostgres")
usePackage("DBI")
usePackage("doParallel")

# Find the number of cores in the system
ncores <- parallel::detectCores()
# Make a cluster and define and file to redirect stdout etc. from workers
cl <-
    parallel::makeCluster(ncores, outfile = paste0(
        "log_trip_inf_",
        format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
        ".txt"
    ))

# Specify the libraries needed by workers
parallel::clusterEvalQ(cl, {
    library(DBI)
    library(RPostgres)
    library(glue)
    library(dplyr)
    con <-
        DBI::dbConnect(
            RPostgres::Postgres(),
            user = "postgres",
            password = Sys.getenv("MAIN_PWD"),
            host = "127.0.0.1",
            port = 5432,
            dbname = "wsdot_evse_main"
        )
})
# Register the cluster
doParallel::registerDoParallel(cl)

LOOKUP_DISTANCE <- 10
CRITICAL_DISTANCE <- 70

main_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = "localhost",
    dbname = "wsdot_evse_main",
    user = Sys.getenv("MAIN_USERNAME"),
    password = Sys.getenv("MAIN_PWD")
)

query_wazip <- "select * from zipcode_record where state = 'WA'"
res_wazip <- DBI::dbSendQuery(main_con, query_wazip)
wa_zip <- DBI::dbFetch(res_wazip)
DBI::dbClearResult(res_wazip)

query_nevses <-
    paste0("select * from new_evses where analysis_id = ", a_id)
res_nevses <- DBI::dbSendQuery(main_con, query_nevses)
nevses <- DBI::dbFetch(res_nevses)
DBI::dbClearResult(res_nevses)

query_bevses <- "select * from built_evse"
res_bevses <- DBI::dbSendQuery(main_con, query_bevses)
bevses <- DBI::dbFetch(res_bevses)
DBI::dbClearResult(res_bevses)

bevses <-
    bevses[, c("bevse_id",
               "longitude",
               "latitude",
               "connector_code",
               "dcfc_count")] %>%
    dplyr::filter(dcfc_count >= 1)

bevses$dcfc_count <- NULL

evses_now <-
    bevses %>% dplyr::mutate(evse_id = paste0("b", bevse_id))
evses_now$bevse_id <- NULL

nevses <-
    nevses[, c("nevse_id",
               "latitude",
               "longitude",
               "chademo_plug_count",
               "combo_plug_count")] %>%
    dplyr::mutate(
        evse_id = paste0("n", nevse_id),
        connector_code = ifelse(
            chademo_plug_count >= 1,
            ifelse(combo_plug_count >= 1, 3, 1),
            ifelse(combo_plug_count >= 1, 2, 0)
        )
    )

nevses$nevse_id <- NULL
nevses$chademo_plug_count <- NULL
nevses$combo_plug_count <- NULL

evses_now <- rbind(evses_now, nevses, overwrite = TRUE)

dbWriteTable(main_con, "evses_now", evses_now)

all_trips_nz_df <-
    vroom::vroom("data-raw/all_trips_non_zero.csv", delim = ",")

all_trips_nz_df <-
    tibble::add_column(all_trips_nz_df, cd_chademo = 0, cd_combo = 0)

count <- 500#nrow(all_trips_nz_df)
cd_chademo_g <- data.frame()
cd_combo_g <- data.frame()

# Specify the variables that the workers need access to
parallel::clusterExport(cl,
                        c(
                            'all_trips_nz_df',
                            'LOOKUP_DISTANCE',
                            'CRITICAL_DISTANCE',
                            'cd_chademo_g'
                        ))

# Find the roads closest to the new evses (one for each)
i = 1
for (i in 1:nrow(nevses)) {
    query_cr <-
        paste0(
            'select id, od_pairs_chademo, od_pairs_combo from "WA_roads", st_distance(geom, st_setsrid(st_makepoint(',
            nevses$longitude[i],
            ', ',
            nevses$latitude[i],
            '), 4326)) as d order by d asc limit 1;'
        )
    cr_row <- DBI::dbGetQuery(main_con, query_cr)
    # If the road has chademo infeasibility and new charger added has chademo plug
    # then update for chademo
    chademo_ods <- cr_row$od_pairs_chademo
    if ((chademo_ods != ' ') &
        (nevses$connector_code[i] %in% c(1, 3))) {
        od_chademo <- strsplit(chademo_ods, split = " ")
        
        # foreach::foreach(
        #     trip_count = 2:length(od_chademo[[1]]),
        #     .inorder = FALSE,
        #     .noexport = "con"
        # ) %dopar% {
        trip_count <- 2
        for (trip_count in 2:length(od_chademo[[1]])) {
            origin <- substr(od_chademo[[1]][trip_count], 1, 5)
            dest <- substr(od_chademo[[1]][trip_count], 6, 10)
            print(paste(
                trip_count,
                "of",
                length(od_chademo[[1]]),
                origin,
                dest
            ))
            
            # First get the ratios of the points that are closest to the charging stations
            # and on the WA_roads, wrt to the shortest_path between a OD pair
            query_chademo <- paste0(
                'select foo.ratios from (select distinct st_linelocatepoint(line, pta) as ratios
      from
      (SELECT
      sp_od2(',
                origin,
                ',',
                dest,
                ') as line,
      ST_SetSRID(ST_MakePoint("longitude", "latitude" ), 4326) as pta
      from evses_now where ("connector_code" = 1 OR "connector_code" = 3) AND st_dwithin(ST_GeogFromWKB(ST_SetSRID(ST_MakePoint("longitude", "latitude" ), 4326)), ST_GeogFromWKB((select sp_od2(',
                origin,
                ',',
                dest,
                '))), cast(',
                LOOKUP_DISTANCE,
                '/0.000621371 as float))) as data ORDER BY ratios ASC) as foo'
            )
            
            res_chademo <-
                DBI::dbSendQuery(main_con, query_chademo)
            rat_chademo <- DBI::dbFetch(res_chademo)
            DBI::dbClearResult(res_chademo)
            
            # Transform these ratios such that we have a new column for ratio successive differences
            rat_chademo <-
                dplyr::as_tibble(rat_chademo) %>% mutate(diff_chademo = ratios - lag(ratios))
            
            # Find the length of the shortest path between OD pair
            query_length <-
                paste0('select st_length(sp_od2(',
                       origin,
                       ',',
                       dest,
                       ')::geography)')
            res_length <-
                DBI::dbSendQuery(main_con, query_length)
            path_length <- DBI::dbFetch(res_length)
            DBI::dbClearResult(res_length)
            
            # Find the length of each segment in the shortest path
            rat_chademo <-
                rat_chademo %>% dplyr::mutate(len_segments = diff_chademo * path_length$st_length * 0.000621371)
            
            # Find the ids of segments that are infeasible
            id_inf <-
                which(rat_chademo$len_segments >= CRITICAL_DISTANCE)
            ids_WA_inf <- vector()
            
            # If there are infeasible segments, then find the road ids
            # that are either overlapping or covered_by the segment
            if (length(id_inf) > 0) {
                for (i in 1:length(id_inf)) {
                    query_segi <-
                        paste0(
                            'select id from public."WA_roads", (select st_linesubstring(line, ',
                            rat_chademo$ratios[id_inf[i] - 1],
                            ' ,',
                            rat_chademo$ratios[id_inf[i]],
                            ') as subs from (select sp_od2(',
                            origin,
                            ' ,',
                            dest,
                            ') as line) as foo ) as subq where (st_overlaps(geom, subs) or st_coveredby(geom, subs))'
                        )
                    res_i <-
                        DBI::dbSendQuery(main_con, query_segi)
                    ids_i <- DBI::dbFetch(res_i)
                    DBI::dbClearResult(res_i)
                    ids_WA_inf <-
                        c(ids_WA_inf, unlist(ids_i))
                    
                }
                
                # Find unique ids since some ids might be repeated
                ids_WA_inf_uniq <- unique(ids_WA_inf)
                print(ids_WA_inf_uniq)
                
                if (length(ids_WA_inf_uniq) >= 1) {
                    # Update the WA_roads table with the respective
                    # trip_count and od_pairs
                    for (iid in 1:length(ids_WA_inf_uniq)) {
                        query_up <- paste0(
                            'insert into wa_trip_inf (road_id, analysis_id, trip_count_chademo, od_pairs_chademo)
        values (', ids_WA_inf_uniq[iid], ', ', a_id, ', ', 
                            all_trips_nz_df$ccounts[all_trips_nz_df$origin == origin &
                                                        all_trips_nz_df$destination == dest],
                            ', ',
                            paste0("'",
                                   origin,
                                   dest,
                                   " '"),
                            ' ) ON CONFLICT (road_id, analysis_id) DO UPDATE SET trip_count_chademo = wa_trip_inf.trip_count_chademo + ', all_trips_nz_df$ccounts[all_trips_nz_df$origin == origin &
                                                                                                                                                       all_trips_nz_df$destination == dest], 
                            ', od_pairs_chademo = wa_trip_inf.od_pairs_chademo || ', paste0("'",
                                                                                origin,
                                                                                dest,
                                                                                " '")
                        )
                        # print(query_up)
                        DBI::dbExecute(main_con, query_up)
                    }
                }
            }
        }
        
    }
}


stopCluster(cl)
stopImplicitCluster()
