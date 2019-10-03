# This file updates the charging distances for an OD pair when new charging stations are added

# handline commandline args...
# rem: with TRUE option below, #args[1] is the "--args" switch; skip it.
# args <- commandArgs(TRUE)
# print(args)

update_cd <- function(config, a_id) {

  # Find the number of cores in the system
  ncores <- parallel::detectCores()
  # Make a cluster and define and file to redirect stdout etc. from workers
  cl <-
    parallel::makeCluster(ncores, outfile = paste0(
      "log_update_cd_",
      format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
      ".txt"
    ))

  # Specify the libraries needed by workers
  parallel::clusterEvalQ(cl, {

    # Database settings -------------------------------------------------------

    main_con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("MAIN_HOST"),
      dbname = Sys.getenv("MAIN_DB"),
      user = Sys.getenv("MAIN_USER"),
      password = Sys.getenv("MAIN_PWD")
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
    password = Sys.getenv("MAIN_PWD")
  )

  # Find EVSES now ----------------------------------------------------------

  # Get all new EVSEs for the said analysis_id where we have some fast charger
  query_nevses <-
    paste0("select * from new_evses where (combo_plug_count + chademo_plug_count) > 0 and analysis_id = ", a_id)

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
    dplyr::filter(dcfc_count >= 1)

  bevses$dcfc_count <- NULL

  # Join the two dataframes to create the EVSES now
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
    parallel::clusterExport(
      cl,
      c(
        'config',
        'cd_chademo_g',
        'cd_combo_g',
        'chademo_ods'
      )
    )

    if (nrow(chademo_ods) > 0) {
      # Find the new charging distance for the OD pairs using the new fleet of EVSEs
      foreach::foreach(
        j = 1:nrow(chademo_ods),
        .inorder = FALSE,
        .noexport = "main_con",
        .combine = "rbind"
      ) %dopar% {
        print(paste0("In chademo j is: ", j, " of ", nrow(chademo_ods)))

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
          LOOKUP_DISTANCE,
          '/0.000621371 as float))) as data ORDER BY ratios ASC) as foo'
          )

        rat_chademo <- DBI::dbGetQuery(main_con, query_chademo)
        print("query_chadmeo done")
        # Find the successive difference in ratios
        rat_chademo <-
          dplyr::as_tibble(rat_chademo) %>% mutate(diff_chademo = ratios - lag(ratios))
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
            ');'
          )

        DBI::dbGetQuery(main_con, query_insert)
        print("query_insert done")
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

    # Specify the variables that the workers need access to
    parallel::clusterExport(
      cl,
      c(
        'LOOKUP_DISTANCE',
        'CRITICAL_DISTANCE',
        'cd_chademo_g',
        'cd_combo_g',
        'combo_ods'
      )
    )

    if (nrow(combo_ods) > 0) {
      # Find the new charging distance for the OD pairs using the new fleet of EVSEs
      foreach::foreach(
        k = 1:nrow(combo_ods),
        .inorder = FALSE,
        .noexport = "main_con",
        .combine = "rbind"
      ) %dopar% {
        print(paste0("In combo k is: ", k, " of ", nrow(combo_ods)))

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
          LOOKUP_DISTANCE,
          '/0.000621371 as float))) as data ORDER BY ratios ASC) as foo'
          )

        rat_combo <- DBI::dbGetQuery(main_con, query_combo)

        print("query_combo done")
        # Find the successive difference in ratios
        rat_combo <-
          dplyr::as_tibble(rat_combo) %>% mutate(diff_combo = ratios - lag(ratios))
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

        }
      }
    }

  DBI::dbRemoveTable(main_con, paste0("evses_now", a_id))

  stopCluster(cl)
  stopImplicitCluster()
  }
