# This file saves the current state of EVSEs into the database

# For level-2 and DCFC

#' Update the EVSEs in the database from the AFDC portal
#'
#' Run tripgen::update_evses() to update the EVSEs table. Will overwrite. Needs AFDC API key.
#'
#' @return TRUE if update was successful
#' @import httr
#' @export

update_evses <- function() {
  # Initialization ------------------

  devtools::load_all("./R/setup_logging.R")
  ## Setup the logging destination
  lg <-
    lgr::get_logger("test")$set_propagate(FALSE)$set_appenders(lgr::AppenderJson$new(layout = LayoutLogstash$new(), file = here::here(
      paste0(
        "logs/update_evses",
        as.character(Sys.Date()),
        ".log"
      )
    )))

  if (!DBI::dbCanConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("MAIN_HOST"),
    dbname = Sys.getenv("MAIN_DB"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"),
    port = Sys.getenv("MAIN_PORT")
  )) {
    lg$log(level = "fatal",
           msg = "Cannot connect to database",
           "ip" = ipify::get_ip())
    # Exit if DB cannot connect
    stop("Cannot connect to database")
  }
  # Database settings -------------------------------------------------------

  main_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("MAIN_HOST"),
    dbname = Sys.getenv("MAIN_DB"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"),
    port = Sys.getenv("MAIN_PORT")
  )

  # Read the EVSE information through the AFDC API
  afdc_url  <-
    paste0(
      "https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?fuel_type=ELEC&state=WA&ev_charging_level=2,dc_fast&status=E&access=public&api_key=",
      Sys.getenv('AFDC_API_KEY')
    )
  evse_dcfc <- readr::read_csv(afdc_url)
  # browser()
  lg$log(
    level = "info",
    msg = paste("Count of WA_EVSEs from AFDC:  ", nrow(evse_dcfc)),
    "ip" = ipify::get_ip()
  )

  evse_dcfc <-
    evse_dcfc[, c(
      'City',
      'State',
      "ZIP",
      "EV Level2 EVSE Num",
      "EV Level1 EVSE Num",
      "EV DC Fast Count",
      "EV Network",
      "Latitude",
      "Longitude",
      "Open Date",
      "EV Connector Types",
      "EV Pricing",
      "ID",
      "Country"
    )] %>%
    dplyr::rename(
      'city' = 'City',
      'state' = 'State',
      'zip' = 'ZIP',
      'level2_count' = "EV Level2 EVSE Num",
      'level1_count' = "EV Level1 EVSE Num",
      "dcfc_count" = "EV DC Fast Count",
      "ev_network" = "EV Network",
      "latitude" = "Latitude",
      "longitude" = "Longitude",
      "open_date" = "Open Date",
      "ev_connector_types" = "EV Connector Types",
      "ev_pricing" = "EV Pricing",
      "bevse_id" = "ID",
      "country" = "Country"
    ) %>% tidyr::replace_na(list('level2_count' = 0, 'dcfc_count' = 0))
  # TODO: Add a fallback clause in the case the API is non-responsive

  evse_dcfc$connector_code <- 0
  # evse_dcfc$charging_cost <- 0

  # Convert the connector type to code for easy parsing in GAMA
  # CHADEMO only - 1
  # J1772COMBO only - 2
  # CHADEMO and J1772COMBO - 3
  # TESLA - 4
  # Ignore J1772 as it is level-2
  for (i in 1:nrow(evse_dcfc)) {
    conns <- evse_dcfc$ev_connector_types[i]
    if (grepl("CHADEMO", conns)) {
      if (grepl("J1772COMBO", conns)) {
        evse_dcfc$connector_code[i] <- 3
      } else {
        evse_dcfc$connector_code[i] <- 1
      }
    } else if (grepl("J1772COMBO", conns)) {
      evse_dcfc$connector_code[i] <- 2
    } else if (grepl("TESLA", conns)) {
      evse_dcfc$connector_code[i] <- 4
    }
  }

  price_df <- readr::read_csv("data-raw/alt_fuel_stations.csv") %>%
    dplyr::select(
      ID,
      FixedChargingPrice,
      ChargingUnit,
      "ChargingValue ($)",
      FixedParkingPrice,
      ParkingUnit,
      ParkingValue
    ) %>%
    tidyr::drop_na() %>%
    dplyr::rename(
      dcfc_fixed_charging_price = FixedChargingPrice,
      dcfc_var_charging_price_unit = ChargingUnit,
      dcfc_var_charging_price = "ChargingValue ($)",
      dcfc_fixed_parking_price = FixedParkingPrice,
      dcfc_var_parking_price_unit = ParkingUnit,
      dcfc_var_parking_price = ParkingValue
    )

  evse_dcfc_with_price <-
    merge(
      evse_dcfc,
      price_df,
      by.x = "bevse_id",
      by.y = "ID",
      all.x = TRUE
    )

  # replace NAs - since it may create issue in GAMA
  evse_dcfc_with_price_noNA <- evse_dcfc_with_price %>%
    tidyr::replace_na(
      list(
        dcfc_fixed_charging_price = 0.0,
        dcfc_var_charging_price_unit = "",
        dcfc_var_charging_price = 0.0,
        dcfc_fixed_parking_price = 0.0,
        dcfc_var_parking_price_unit = "",
        dcfc_var_parking_price = 0.0
      )
    )

  evse_dcfc_with_price_noNA$in_service <- TRUE

  lg$log(
    level = "info",
    msg = paste("Count of WA_EVSEs before insert:  ", nrow(evse_dcfc_with_price_noNA)),
    "ip" = ipify::get_ip()
  )

  DBI::dbBegin(main_con)
  DBI::dbWriteTable(main_con,
                    "built_evse",
                    evse_dcfc_with_price_noNA,
                    overwrite = TRUE)

  res <- main_con %>% DBI::dbSendQuery(
    paste0(
      "insert into table_stats (table_name, last_updated) values ('built_evse', '",
      as.character(Sys.time()),
      "') on conflict (table_name) do update set last_updated = EXCLUDED.last_updated;"
    )
  )
  DBI::dbClearResult(res)
  DBI::dbCommit(main_con)
  DBI::dbDisconnect(main_con)

}
