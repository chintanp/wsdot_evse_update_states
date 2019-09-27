# This file updates the charging distances for an OD pair when new charging stations are added

# handline commandline args...
# rem: with TRUE option below, #args[1] is the "--args" switch; skip it.
# args <- commandArgs(TRUE)
# print(args)
a_id <- 14 #args[2]

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
  main_con <-
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

# Make a unique table for each aalysis_id and drop if after the analysis is complete
# dbWriteTable(main_con, "evses_now", evses_now)

count <- 500#nrow(all_trips_nz_df)
cd_chademo_g <- data.frame()
cd_chademo_g$ST_Length <- NULL
cd_chademo_g$subs <- NULL
cd_combo_g <- data.frame()
cd_combo_g$ST_Length <- NULL
cd_combo_g$subs <- NULL

# Specify the variables that the workers need access to
parallel::clusterExport(cl,
                        c('LOOKUP_DISTANCE',
                          'CRITICAL_DISTANCE',
                          'cd_chademo_g'))

# Find the roads closest to the new evses (one for each)
i = 1
for (i in 1:nrow(nevses)) {
  query_od_chademo <-
    paste0(
      'select origin, destination from od_cd where st_dwithin(cd_chademo_geog, st_setsrid(st_makepoint(',
      nevses$longitude[i],
      ', ',
      nevses$latitude[i],
      '), 4326)::geography, ',
      LOOKUP_DISTANCE,
      '/0.000621371)'
    )
  
  # Get all the OD pairs who charging-distance geo is within 10 miles of the new charging station location
  chademo_ods <- DBI::dbGetQuery(main_con, query_od_chademo)
  
  if (nrow(chademo_ods) > 0) {
    # Find the new charging distance for the OD pairs using the new fleet of EVSEs
    for (j in 1:nrow(chademo_ods)) {
      orig_j <- chademo_ods$origin[j]
      dest_j <- chademo_ods$destination[j]
      
      # Find the ratios of the points on the shortest path closest the charging stations
      query_chademo <- paste0(
        'select foo.ratios from (select distinct st_linelocatepoint(line, pta) as ratios
      from
      (SELECT
      sp_od2(',
        orig_j,
        ',',
        dest_j,
        ') as line,
      ST_SetSRID(ST_MakePoint("longitude", "latitude" ), 4326) as pta
      from evses_now where ("connector_code" = 1 OR "connector_code" = 3) AND st_dwithin(ST_GeogFromWKB(ST_SetSRID(ST_MakePoint("longitude", "latitude" ), 4326)), ST_GeogFromWKB((select sp_od2(',
        orig_j,
        ',',
        dest_j,
        '))), cast(',
        LOOKUP_DISTANCE,
        '/0.000621371 as float))) as data ORDER BY ratios ASC) as foo'
      )
      
      rat_chademo <- DBI::dbGetQuery(main_con, query_chademo)
      # Find the successive difference in ratios
      rat_chademo <-
        dplyr::as_tibble(rat_chademo) %>% mutate(diff_chademo = ratios - lag(ratios))
      # Find the index of the ratio with maximum difference - this is the maximum
      # spacing between charging stations along the route
      max_index <- which.max(rat_chademo$diff_chademo)
      
      if (max_index > 0) {
        ind_maxes <- c(max_index - 1, max_index)
        
        query_chademo2 <- paste0(
          'select ST_Length(ST_GeographyFromText(subs)), subs
        from (select st_astext(st_LineSubstring(line, pta, ptb)) as subs
        from (select sp_od2(',
          orig_j,
          ',',
          dest_j,
          ') as line,',
          rat_chademo$ratios[ind_maxes[1]],
          ' as pta, ',
          rat_chademo$ratios[ind_maxes[2]],
          '
        as ptb)
        as data)
        as foo'
        )
      }
      
      cd_chademo_g <- DBI::dbGetQuery(main_con, query_chademo2)
      
      query_insert <-
        paste0(
          'insert into od_cd (origin, destination, cd_chademo, cd_chademo_geog, analysis_id) values (',
          orig_j,
          ', ',
          dest_j,
          ', ',
          cd_chademo_g$ST_Length,
          ', ',
          cd_chademo_g$subs,
          ', ',
          a_id,
          ');'
        )
      
      
    }
  }
  
  query_combo_od <-
    paste0(
      'select origin, destination from od_cd where st_dwithin(cd_combo_geog, st_setsrid(st_makepoint(',
      nevses$longitude[i],
      ', ',
      nevses$latitude[i],
      '), 4326)::geography, ',
      LOOKUP_DISTANCE,
      '/0.000621371)'
    )
  
  # Get all the OD pairs who charging-distance geo is within 10 miles of the new charging station location
  combo_ods <- DBI::dbGetQuery(main_con, query_combo_od)
  
  if (nrow(combo_ods) > 0) {
    # Find the new charging distance for the OD pairs using the new fleet of EVSEs
    for (k in 1:nrow(combo_ods)) {
      orig_k <- combo_ods$origin[k]
      dest_k <- combo_ods$destination[k]
      
      # Find the ratios of the points on the shortest path closest the charging stations
      query_combo <- paste0(
        'select foo.ratios from (select distinct st_linelocatepoint(line, pta) as ratios
      from
      (SELECT
      sp_od2(',
        orig_k,
        ',',
        dest_k,
        ') as line,
      ST_SetSRID(ST_MakePoint("longitude", "latitude" ), 4326) as pta
      from evses_now where ("connector_code" = 2 OR "connector_code" = 3) AND st_dwithin(ST_GeogFromWKB(ST_SetSRID(ST_MakePoint("longitude", "latitude" ), 4326)), ST_GeogFromWKB((select sp_od2(',
        orig_k,
        ',',
        dest_k,
        '))), cast(',
        LOOKUP_DISTANCE,
        '/0.000621371 as float))) as data ORDER BY ratios ASC) as foo'
      )
      
      rat_combo <- DBI::dbGetQuery(main_con, query_combo)
      # Find the successive difference in ratios
      rat_combo <-
        dplyr::as_tibble(rat_combo) %>% mutate(diff_combo = ratios - lag(ratios))
      # Find the index of the ratio with maximum difference - this is the maximum
      # spacing between charging stations along the route
      max_index <- which.max(rat_combo$diff_combo)
      
      if (max_index > 0) {
        ind_maxes <- c(max_index - 1, max_index)
        
        query_combo2 <- paste0(
          'select ST_Length(ST_GeographyFromText(subs)), subs
        from (select st_astext(st_LineSubstring(line, pta, ptb)) as subs
        from (select sp_od2(',
          orig_k,
          ',',
          dest_k,
          ') as line,',
          rat_combo$ratios[ind_maxes[1]],
          ' as pta, ',
          rat_combo$ratios[ind_maxes[2]],
          '
        as ptb)
        as data)
        as foo'
        )
      }
      
      cd_combo_g <-
        DBI::dbGetQuery(main_con, query_combo2)
      
      query_update <- paste0('update od_cd set cd_combo = ', cd_combo_g$ST_Length, ', cd_combo_geog = ', cd_combo_g$subs, ' where analysis_id = ', a_id, ' and origin = ', orig_k, ' and destination = ', dest_k, ';')
      
      DBI::dbGetQuery(main_con, query_update)
      
    }
  }
}




# stopCluster(cl)
# stopImplicitCluster()
