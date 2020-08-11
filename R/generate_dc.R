#' Find out whether a zip code has destination charger in 10 mile radius
#'
#'
#' @return
#'
#' @export
#'
#' @import magrittr
#' @importFrom utils data
#' @importFrom rlang .data
#'
create_dc <- function() {
  main_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("MAIN_HOST"),
    dbname = Sys.getenv("MAIN_DB"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"),
    port = Sys.getenv("MAIN_PORT")
  )

  query_wazip <- "select * from zipcode_record where state = 'WA'"
  wa_zip <- DBI::dbGetQuery(main_con, query_wazip)

  for (zip_count in 1:nrow(wa_zip)) {
    print(paste(zip_count, "of", nrow(wa_zip)))
    query_chademo <-
      paste0(
        'select bool_or(st_dwithin( st_setsrid(st_makepoint(b.longitude, b.latitude), 4326)::geography, st_setsrid(st_makepoint(',
        wa_zip$longitude[zip_count],
        ', ',
        wa_zip$latitude[zip_count],
        '), 4326)::geography, 10/0.000621371) ) from built_evse as b where connector_code = 1 or connector_code = 3'
      )
    dc_chademo <- DBI::dbGetQuery(main_con, query_chademo)

    query_combo <-
      paste0(
        'select bool_or(st_dwithin( st_setsrid(st_makepoint(b.longitude, b.latitude), 4326)::geography, st_setsrid(st_makepoint(',
        wa_zip$longitude[zip_count],
        ', ',
        wa_zip$latitude[zip_count],
        '), 4326)::geography, 10/0.000621371) ) from built_evse as b where connector_code = 2 or connector_code = 3'
      )
    dc_combo <- DBI::dbGetQuery(main_con, query_combo)

    query_l2 <-
      paste0(
        'select bool_or(st_dwithin( st_setsrid(st_makepoint(b.longitude, b.latitude), 4326)::geography, st_setsrid(st_makepoint(',
        wa_zip$longitude[zip_count],
        ', ',
        wa_zip$latitude[zip_count],
        '), 4326)::geography, 10/0.000621371) ) from built_evse as b where level2_count >= 1'
      )
    dc_l2 <- DBI::dbGetQuery(main_con, query_l2)

    dc_row <- data.frame(
      "zip" = wa_zip$zip[zip_count],
      "dc_chademo" = dc_chademo$bool_or,
      "dc_combo" = dc_combo$bool_or,
      "dc_level2" = dc_l2$bool_or,
      "analysis_id" = -1
    )

    DBI::dbAppendTable(main_con, "dest_charger", dc_row, append = TRUE)

  }
}
