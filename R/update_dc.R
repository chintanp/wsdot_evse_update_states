#' Find if there is a destination charger
#'
#'
#' @param a_id analysis_id
#'
#' @return
#'
#' @export
#'
#' @import magrittr
#' @importFrom utils data
#' @importFrom rlang .data
#'
update_dc <- function(a_id = 1) {

  # Initialization ------------------

  devtools::load_all("./R/setup_logging.R")
  ## Setup the logging destination
  lg <-
    lgr::get_logger("test")$set_propagate(FALSE)$set_appenders(lgr::AppenderJson$new(layout = LayoutLogstash$new(), file = here::here(
      paste0("logs/runner_", as.character(a_id), ".log")
    )))
  # Database settings -------------------------------------------------------

  main_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("MAIN_HOST"),
    dbname = Sys.getenv("MAIN_DB"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"),
    port = Sys.getenv("MAIN_PORT")
  )
  # print(a_id)

  print(a_id)
  query_wazip <- "select * from zipcode_record where state = 'WA'"
  res_wazip <- DBI::dbSendQuery(main_con, query_wazip)
  wa_zip <- DBI::dbFetch(res_wazip)
  DBI::dbClearResult(res_wazip)

  query_nevses <-
    paste0("select * from new_evses where dcfc_plug_count > 0 and analysis_id = ",
           a_id)
  print(query_nevses)
  res_nevses <- DBI::dbSendQuery(main_con, query_nevses)
  nevses <- DBI::dbFetch(res_nevses)
  DBI::dbClearResult(res_nevses)

  if (nrow(nevses) > 0) {
    for (i in 1:nrow(nevses)) {
      query_dwithin <-
        paste0(
          "select zip, st_dwithin(st_setsrid(st_makepoint(w.longitude, w.latitude), 4326)::geography, st_setsrid(st_makepoint(",
          nevses$longitude[i],
          ",",
          nevses$latitude[i],
          "), 4326)::geography, 10/0.000621371) from (select * from zipcode_record where state = 'WA') as w"
        )
      idwithin <- DBI::dbGetQuery(main_con, query_dwithin)
      # Check if rows are returned from the query

      idwithin$dc_chademo <- FALSE
      idwithin$dc_combo <- FALSE
      idwithin$dc_level2 <- FALSE

      if (nevses$dcfc_plug_count[i] >= 1) {
        # If any chademo chargers are added, then the dc_chademo is updated
        if (nevses$connector_code[i] == 1) {
          idwithin$dc_chademo <- idwithin$st_dwithin
        } else if (nevses$connector_code[i] == 2) {
          idwithin$dc_combo <- idwithin$st_dwithin
        } else if (nevses$connector_code[i] == 3) {
          idwithin$dc_combo <- idwithin$st_dwithin
          idwithin$dc_chademo <- idwithin$st_dwithin
        }
      }

      if (nevses$level2_plug_count[i] >= 1) {
        idwithin$dc_level2 <- idwithin$st_dwithin
      }
      idwithin <- idwithin %>% dplyr::filter(st_dwithin == TRUE)

      if (nrow(idwithin) >= 1) {
        idwithin$analysis_id <- a_id
        # Once dc_chademo and dc_combo are updated, st_dwithin column can be deleted
        idwithin$st_dwithin <- NULL
        DBI::dbAppendTable(main_con, "dest_charger", idwithin, append = TRUE)
      }
    }
  }

  DBI::dbDisconnect(main_con)
}
