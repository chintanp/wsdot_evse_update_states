# This file updates the charging distances for an OD pair when new charging stations are added

# handline commandline args...
# rem: with TRUE option below, #args[1] is the "--args" switch; skip it.
# args <- commandArgs(TRUE)
# print(args)

globalVariables('j')
globalVariables('k')
requireNamespace("foreach")
#' Update the charging distances
#'
#' This function `update_cd()` updates the charging distances given the new set of charging stations.
#'
#' @param config constants
#' @param a_id analysis_id
#'
#' @return
#'
#' @export
#'
#' @import magrittr
#' @importFrom rlang .data
#' @importFrom utils tail
#' @import foreach
#'
update_cd <- function(config, a_id = 15) {
  library(dplyr)
  library(magrittr)
  library(foreach)
  library(ipify)
  library(lgr)

  lg <- get_logger("test")$set_propagate(FALSE)$set_appenders(lgr::AppenderJson$new(layout = LayoutLogstash$new(), file = here::here(paste0("logs/runner_", as.character(a_id), ".log"))))

  # print("In update_cd")
  requireNamespace("foreach")
  # print(str(config))
  # print(a_id)
  if (is.na(a_id)) {
    print("missing indeed")
    lg$log(
      level = "fatal",
      msg = "Analysis_id not passed in charging distance calc",
      "ip" = get_ip()
    )
    # a_id  <-  99
  }
  # print(a_id)
  # Find the number of cores in the system
  ncores <- parallel::detectCores()
  # Make a cluster and define and file to redirect stdout etc. from workers
  cl <-
    parallel::makeCluster(ncores, outfile = paste0("log_update_cd_",
      format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
      ".txt"
    ))

  # Specify the libraries needed by workers
  parallel::clusterEvalQ(cl, {
    library(DBI)
    library(RPostgres)
    library(glue)
    library(dplyr)
    library(doParallel)
    library(ipify)
    library(lgr)


    # Database settings -------------------------------------------------------

    main_con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("MAIN_HOST"),
      dbname = Sys.getenv("MAIN_DB"),
      user = Sys.getenv("MAIN_USER"),
      password = Sys.getenv("MAIN_PWD"),
      port = Sys.getenv("MAIN_PORT")
    )

  })
  # Register the cluster
  doParallel::registerDoParallel(cl)

  # LOOKUP_DISTANCE <- 10
  # CRITICAL_DISTANCE <- 70

  # Database settings -------------------------------------------------------

  main_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("MAIN_HOST"),
    dbname = Sys.getenv("MAIN_DB"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"),
    port = Sys.getenv("MAIN_PORT")
  )

  # Find EVSES now ----------------------------------------------------------

  # Get all new EVSEs for the said analysis_id where we have some fast charger
  query_nevses <-
    paste0("select * from new_evses where dcfc_plug_count > 0 and analysis_id = ",
           a_id)

  nevses <- DBI::dbGetQuery(main_con, query_nevses)

  # Get all the built evses
  query_bevses <- "select * from built_evse"

  bevses <- DBI::dbGetQuery(main_con, query_bevses)

  # Select the necessary columns and rows with DCFC, as we are collecting Level2 as well
  bevses <-
    bevses[, c("bevse_id",
               "longitude",
               "latitude",
               "connector_code",
               "dcfc_count")] %>%
    dplyr::filter(.data$dcfc_count >= 1)

  bevses$dcfc_count <- NULL

  # Join the two dataframes to create the EVSES now
  evses_now <-
    bevses %>% dplyr::mutate(evse_id = paste0("b", .data$bevse_id))
  evses_now$bevse_id <- NULL

  nevses <-
    nevses[, c("nevse_id",
               "latitude",
               "longitude",
               "connector_code")] %>%
    dplyr::mutate(evse_id = paste0("n", .data$nevse_id))

  nevses$nevse_id <- NULL

  evses_now <- rbind(evses_now, nevses)

  # Create a table with total evses
  DBI::dbWriteTable(main_con, paste0("evses_now", a_id), evses_now, overwrite = FALSE)
  # Make a unique table for each aalysis_id and drop if after the analysis is complete
  # dbWriteTable(main_con, "evses_now", evses_now)

  count <- 500#nrow(all_trips_nz_df)
  cd_chademo_g <- data.frame()
  cd_chademo_g$ST_Length <- NULL
  cd_chademo_g$subs <- NULL
  cd_combo_g <- data.frame()

  i = 1
  # Iterate through the new chargers
  if (nrow(nevses) > 0) {
    for (i in 1:nrow(nevses)) {
      # Find the charging distance geometry that is within the lookup distance from charging station
      query_od_chademo <-
        paste0(
          'select origin, destination from od_cd where st_dwithin(cd_chademo_geog, st_setsrid(st_makepoint(',
          nevses$longitude[i],
          ', ',
          nevses$latitude[i],
          '), 4326)::geography, ',
          config$LOOKUP_DISTANCE,
          '/0.000621371)'
        )

      # Get all the OD pairs who charging-distance geo is within 10 miles of the new charging station location
      chademo_ods <- DBI::dbGetQuery(main_con, query_od_chademo)

      # Specify the variables that the workers need access to
      parallel::clusterExport(cl,
                              c('config',
                                'cd_chademo_g',
                                'cd_combo_g',
                                'chademo_ods',
                                'a_id',
                                'lg'),
                              envir = environment())

      if (nrow(chademo_ods) > 0) {
        # Find the new charging distance for the OD pairs using the new fleet of EVSEs
        foreach::foreach(
          j = 1:nrow(chademo_ods),
          .inorder = FALSE,
          .noexport = "main_con",
          .combine = "rbind"
        ) %dopar% {
          print(paste0("In chademo j is: ", j, " of ", nrow(chademo_ods)))
          print(paste0("analysis_id: ", a_id))


          lg$log(
            level = "info",
            msg = paste0("In chademo j is: ", j, " of ", nrow(chademo_ods)),
            "ip" = get_ip(),
            "status" = "beginning processing"
          )

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
          from ',
            paste0("evses_now", a_id),
            ' where ("connector_code" = 1 OR "connector_code" = 3) AND st_dwithin(ST_GeogFromWKB(ST_SetSRID(ST_MakePoint("longitude", "latitude" ), 4326)), ST_GeogFromWKB((select sp_od2(',
            orig_j,
            ',',
            dest_j,
            '))), cast(',
            config$LOOKUP_DISTANCE,
            '/0.000621371 as float))) as data ORDER BY ratios ASC) as foo'
          )

          rat_chademo <- DBI::dbGetQuery(main_con, query_chademo)
          print("query_chadmeo done")
          lg$log(
            level = "info",
            msg = paste0("In chademo j is: ", j, " of ", nrow(chademo_ods)),
            "ip" = get_ip(),
            "status" = "query_chademo done"
          )

          # Find the successive difference in ratios
          rat_chademo <-
            dplyr::as_tibble(rat_chademo) %>% dplyr::mutate(diff_chademo = .data$ratios - dplyr::lag(.data$ratios))
          # Find the index of the ratio with maximum difference - this is the maximum
          # spacing between charging stations along the route
          max_index <- which.max(rat_chademo$diff_chademo)

          if (max_index > 0) {
            ind_maxes <- c(max_index - 1, max_index)

            query_chademo2 <- paste0(
              'select ST_Length(subs::geography), subs::geography
            from (select st_LineSubstring(line, pta, ptb) as subs
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

          print("query_chadmeo2 done")
          lg$log(
            level = "info",
            msg = paste0("In chademo j is: ", j, " of ", nrow(chademo_ods)),
            "ip" = get_ip(),
            "status" = "query_chademo2 done"
          )

          query_insert <-
            paste0(
              'insert into od_cd (origin, destination, cd_chademo, cd_chademo_geog, analysis_id) values (',
              orig_j,
              ', ',
              dest_j,
              ', ',
              cd_chademo_g$st_length * 0.000621371,
              ", '",
              cd_chademo_g$subs,
              "', ",
              a_id,
              ') on conflict on constraint od_cd_pkey do update set cd_chademo = EXCLUDED.cd_chademo, cd_chademo_geog = EXCLUDED.cd_chademo_geog;'
            )

          DBI::dbGetQuery(main_con, query_insert)
          print("query_insert done")
          lg$log(
            level = "info",
            msg = paste0("In chademo j is: ", j, " of ", nrow(chademo_ods)),
            "ip" = get_ip(),
            "status" = "query_insert done"
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
          config$LOOKUP_DISTANCE,
          '/0.000621371)'
        )

      # Get all the OD pairs who charging-distance geo is within 10 miles of the new charging station location
      combo_ods <- DBI::dbGetQuery(main_con, query_combo_od)

      # Specify the variables that the workers need access to
      parallel::clusterExport(cl,
                              c('config',
                                'cd_chademo_g',
                                'cd_combo_g',
                                'combo_ods'),
                              envir = environment())

      if (nrow(combo_ods) > 0) {
        # Find the new charging distance for the OD pairs using the new fleet of EVSEs
        foreach::foreach(
          k = 1:nrow(combo_ods),
          .inorder = FALSE,
          .noexport = "main_con",
          .combine = "rbind"
        ) %dopar% {

          print(paste0("In combo k is: ", k, " of ", nrow(combo_ods)))

          lg$log(
            level = "info",
            paste0("In combo k is: ", k, " of ", nrow(combo_ods)),
            "ip" = get_ip(),
            "status" = "beginning processing"
          )

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
          from ',
            paste0("evses_now", a_id),
            ' where ("connector_code" = 2 OR "connector_code" = 3) AND st_dwithin(ST_GeogFromWKB(ST_SetSRID(ST_MakePoint("longitude", "latitude" ), 4326)), ST_GeogFromWKB((select sp_od2(',
            orig_k,
            ',',
            dest_k,
            '))), cast(',
            config$LOOKUP_DISTANCE,
            '/0.000621371 as float))) as data ORDER BY ratios ASC) as foo'
          )

          rat_combo <- DBI::dbGetQuery(main_con, query_combo)

          print("query_combo done")
          lg$log(
            level = "info",
            paste0("In combo k is: ", k, " of ", nrow(combo_ods)),
            "ip" = get_ip(),
            "status" = "query_combo done"
          )

          # Find the successive difference in ratios
          rat_combo <-
            dplyr::as_tibble(rat_combo) %>% dplyr::mutate(diff_combo = .data$ratios - dplyr::lag(.data$ratios))
          # Find the index of the ratio with maximum difference - this is the maximum
          # spacing between charging stations along the route
          max_index <- which.max(rat_combo$diff_combo)

          if (max_index > 0) {
            ind_maxes <- c(max_index - 1, max_index)

            query_combo2 <- paste0(
              'select ST_Length(subs::geography), subs::geography
            from (select st_LineSubstring(line, pta, ptb) as subs
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

          print("query_combo2 done")
          lg$log(
            level = "info",
            paste0("In combo k is: ", k, " of ", nrow(combo_ods)),
            "ip" = get_ip(),
            "status" = "query_combo2 done"
          )

          query_update <-
            paste0(
              'update od_cd set cd_combo = ',
              cd_combo_g$st_length * 0.000621371,
              ", cd_combo_geog = '",
              cd_combo_g$subs,
              "' where analysis_id = ",
              a_id,
              ' and origin = ',
              orig_k,
              ' and destination = ',
              dest_k,
              ';'
            )

          DBI::dbGetQuery(main_con, query_update)
          print("query_update done")
          lg$log(
            level = "info",
            paste0("In combo k is: ", k, " of ", nrow(combo_ods)),
            "ip" = get_ip(),
            "status" = "query_update done"
          )

        }
      }
    }
  }

  DBI::dbRemoveTable(main_con, paste0("evses_now", a_id))

  parallel::stopCluster(cl)
  doParallel::stopImplicitCluster()

  DBI::dbDisconnect(main_con)
}
