# This file reads the data from the WA DOL API
# and download the list of currently registered EV in WA

#' Email a user a report is ready
#'
#' Requires an account at Mailgun: https://mailgun.com
#' Pre-verification can only send to a whitelist of emails you configure
#'
#' @param email Email to send to
#' @param mail_message Any extra info
#'
#' @return TRUE if successful email sent
#'
#' @export
#'
#' @import httr
#' @export
sendEmail <- function(email = "cp84@uw.edu",
                      mail_message = "WA_BEVS have NAs") {
  url <-
    "https://api.mailgun.net/v3/sandbox4b565673c15246d8b4f6f15176a7e3fc.mailgun.org/messages"
  ## username:password so api_key is all after the api:
  api_key <- Sys.getenv("MAILGUN_API_KEY")
  the_body <-
    list(
      from = "Mailgun Sandbox <postmaster@sandbox4b565673c15246d8b4f6f15176a7e3fc.mailgun.org>",
      to = email,
      subject = "WA_BEVS have NAs",
      text = mail_message
    )

  req <- httr::POST(url,
                    httr::authenticate("api", api_key),
                    encode = "form",
                    body = the_body)

  httr::stop_for_status(req)

  TRUE
}

#' Update the BEVs in the database by fetching the latest from WADOL and overwriting
#'
#' Run tripgen::update_wa_bevs() to update the BEVs in the state.
#'
#' @return TRUE if update was successful
#' @import httr
#' @export
update_wa_bevs <- function() {
  # Initialization ------------------

  devtools::load_all("./R/setup_logging.R")
  ## Setup the logging destination
  lg <-
    lgr::get_logger("test")$set_propagate(FALSE)$set_appenders(lgr::AppenderJson$new(layout = LayoutLogstash$new(), file = here::here(
      paste0("logs/update_bevs", as.character(Sys.Date()), ".log")
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

  # Download EV data from WA DOL - needs an SOCRATA APP TOKEN.
  wa_evs <-
    RSocrata::read.socrata("https://data.wa.gov/resource/f6w7-q2d2.csv",
                           app_token = Sys.getenv('SOCRATA_APP_TOKEN'))
  lg$log(
    level = "info",
    msg = paste("Count of WA_EVs downloaded from socrata:  ", nrow(wa_evs)),
    "ip" = ipify::get_ip()
  )

  # Filter out BEVs
  wa_bevs <- wa_evs %>% tibble::as_tibble() %>%
    dplyr::filter(agrepl("Battery Electric Vehicle (BEV)", .data$ev_type))

  lg$log(
    level = "info",
    msg = paste("Count of WA_BEVs after filtering:  ", nrow(wa_bevs)),
    "ip" = ipify::get_ip()
  )

  wa_bevs <-
    wa_bevs %>% dplyr::select(-c('vin_1_10', 'cafv_type', 'ev_type', 'geocoded_column')) %>% dplyr::rename('veh_id' = 'dol_vehicle_id')

  # Create latitude and kongitude column from the "Geocoded Column"
  # wa_bevs$wa_ll <- as.data.frame(sf::st_coordinates(sf::st_as_sfc(wa_bevs$geocoded_column)))
  # wa_bevs <- wa_bevs %>% dplyr::mutate(latitude = .data$wa_ll$Y, longitude = .data$wa_ll$X) %>%
  #     dplyr::select(-.data$wa_ll)

  # Get fuel-economy data from fueleconomy.gov
  tmp <- tempfile(fileext = ".zip")
  download.file("https://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip",
                tmp,
                quiet = TRUE)
  unzip(tmp, exdir = "data-raw")

  raw_vehicles_data <- read.csv("data-raw/vehicles.csv")

  ev_tbl <- raw_vehicles_data %>%
    tibble::as_tibble() %>%
    dplyr::select(
      .data$id,
      .data$make,
      .data$model,
      .data$year,
      fuel = fuelType,
      fuel_consumption = .data$combE,
      .data$range
    ) %>%
    dplyr::mutate(make = tolower(make), model = tolower(model)) %>%
    dplyr::filter(.data$fuel == "Electricity") %>%
    dplyr::arrange(make, model, year)

  # Getting electric vehicles data from AFDC
  # afdc_vehicles <-
  #     read.csv("https://afdc.energy.gov/vehicles/search/results.csv?current=true", header = TRUE, sep = ",", stringsAsFactors = FALSE)
  #
  #
  # afdc_evs <- afdc_vehicles %>% dplyr::filter(.data$Fuel == "Electric")
  # afdc_evs[is.na(afdc_evs)] <- 0

  wa_bevs$range <- NA
  wa_bevs$capacity <- NA
  wa_bevs$fuel_consumption <- NA

  for (i in 1:nrow(wa_bevs)) {
    cur_make <- tolower(wa_bevs$make[i])
    cur_model <- tolower(wa_bevs$model[i])
    cur_year <- wa_bevs$model_year[i]
    # if (cur_make == 'Jaguar') {
    #   print(i)
    #   print(cur_make)
    #   print(cur_model)
    #   print(cur_year)
    #   browser()
    # }

    ev_df <-
      subset(ev_tbl,
             year == cur_year &
               agrepl(cur_make, make) &
               agrepl(cur_model, model))
    # print(ev_df)
    if (is.data.frame(ev_df) && nrow(ev_df) == 0) {
      wa_bevs$range[i] <- NA
      wa_bevs$capacity[i] <- NA
    } else {
      wa_bevs$range[i] <- mean(ev_df$range)
      wa_bevs$capacity[i] <-
        mean(ev_df$range) * mean(ev_df$fuel_consumption) / 100
      wa_bevs$fuel_consumption[i] <- mean(ev_df$fuel_consumption)
    }
  }

  # browser()
  #
  # wa_bevs_man <- wa_bevs
  # ## Add data manually for problem cases
  # # Smart fortwo
  # wa_bevs_man$range[which(wa_bevs_man$model == "Smart Fortwo Electric Drive")] <-
  #     76
  # wa_bevs_man$capacity[which(wa_bevs_man$model == "Smart Fortwo Electric Drive")] <-
  #     76 * 32 / 100
  # wa_bevs_man$fuel_consumption[which(wa_bevs_man$model == "Smart Fortwo Electric Drive")] <-
  #     32

  wa_bevs_new <- wa_bevs %>% dplyr::filter(.data$model_year > 2008)
  wa_bevs_man <- wa_bevs_new
  # Remove Tesla Roadster
  wa_bevs_man <-
    wa_bevs_man[-which(tolower(wa_bevs_man$model) == "roadster"),]

  wa_bevs_man$range[which(tolower(wa_bevs_man$make) == "azure dynamics")] <- 56
  wa_bevs_man$capacity[which(tolower(wa_bevs_man$make) == "azure dynamics")] <-
    56 * 54 / 100
  wa_bevs_man$fuel_consumption[which(tolower(wa_bevs_man$make) == "azure dynamics")] <-
    54

  # Toyota RAV4
  wa_bevs_man$range[which(tolower(wa_bevs_man$make) == "toyota" &
                            tolower(wa_bevs_man$model) == "rav4 ev 2wd")] <-
    103
  wa_bevs_man$capacity[which(tolower(wa_bevs_man$make) == "toyota" &
                               tolower(wa_bevs_man$model) == "rav4 ev 2wd")] <-
    103 * 46 / 100
  wa_bevs_man$fuel_consumption[which(tolower(wa_bevs_man$make) == "toyota" &
                                       tolower(wa_bevs_man$model) == "rav4 ev 2wd")] <-
    46

  ## Manually assigning the Connector type codes
  wa_bevs_man$connector_code <- NA
  # Tesla is 4
  wa_bevs_man$connector_code[which(tolower(wa_bevs_man$make) == "tesla")] <-
    4

  # BMW, Chevrolet, Mercedes, Volkswagen are SAE Combo - 2
  combo_connector_makes <-
    tolower(c(
      'BMW',
      'Volkswagen',
      'Chevrolet',
      'Mercedes-Benz',
      'Ford',
      'Jaguar',
      'Audi',
      'MINI'
    ))
  wa_bevs_man$connector_code[which(tolower(wa_bevs_man$make) %in% combo_connector_makes)] <-
    2

  # Nissan, Mitsubishi, etc. are ChaDemo - 1
  chademo_connector_makes <-
    tolower(c('Nissan', 'Mitsubishi', 'Toyota', 'Kia', 'Honda', 'Hyundai'))
  wa_bevs_man$connector_code[which(tolower(wa_bevs_man$make) %in% chademo_connector_makes)] <-
    1

  # Remove EVs that do not have fast charging
  wa_bevs_man2 <-
    wa_bevs_man %>%
    dplyr::filter(!(tolower(.data$make) == "fiat")) %>%
    dplyr::filter(!(tolower(.data$model) == "coda")) %>%
    dplyr::filter(!(tolower(.data$make) == "azure dynamics")) %>%
    dplyr::filter(!(tolower(.data$make) == "smart")) %>%
    dplyr::filter(!(tolower(.data$model) == "life")) %>%
    dplyr::filter(!(tolower(.data$make) == "th!nk"))

  # Some NAs remain in `base_msrp` and `legislative_district`
  wa_bevs_man3 <-
    wa_bevs_man2 %>% tidyr::replace_na(replace = list(base_msrp = 0, legislative_district = 0))

  # Send an email if some rows are incomplete, i.e. logic above fails
  wa_bevs_incomp <- wa_bevs_man3[!complete.cases(wa_bevs_man3), ]
  if (nrow(wa_bevs_incomp) > 0) {
      sendEmail(mail_message = toString(wa_bevs_incomp))
  }

  wa_bevs_man4 <- wa_bevs_man3[complete.cases(wa_bevs_man3), ]


  wa_bevs_man4 <- wa_bevs_man4 %>% dplyr::rename('range_fe' = 'range')

  # Convert veh_id to char
  wa_bevs_man4$veh_id <- as.character(wa_bevs_man4$veh_id)

  lg$log(
    level = "info",
    msg = paste(
      "Count of relevant WA_BEVs after processing  ",
      nrow(wa_bevs_man4)
    ),
    "ip" = ipify::get_ip()
  )
  DBI::dbBegin(main_con)
  DBI::dbWriteTable(main_con, "wa_bevs", wa_bevs_man4, overwrite = TRUE)

  res <- main_con %>% DBI::dbSendQuery(
    paste0(
      "insert into table_stats (table_name, last_updated) values ('wa_bevs', '",
      as.character(Sys.time()),
      "') on conflict (table_name) do update set last_updated = EXCLUDED.last_updated;"
    )
  )
  DBI::dbClearResult(res)
  DBI::dbCommit(main_con)
  DBI::dbDisconnect(main_con)

}


# DBI::dbAppendTable(main_con, name = "wa_bevs", wa_bevs)
#
# DBI::dbDisconnect(main_con)
