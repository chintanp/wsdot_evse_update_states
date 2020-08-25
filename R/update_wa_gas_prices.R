# THis file reads the data from gas buddy API and saves it to a csv etc.


###################
# The gas buddy API is undocumented and unsupported.
# This method of gas prices update is not stable and is subject to errors.
###################

#' Email a user a report is ready
#'
#' Requires an account at Mailgun: https://mailgun.com
#' Pre-verification can only send to a whitelist of emails you configure
#'
#' @param email Email to send to
#' @param mail_message Any extra info
#'
#' @return TRUE if successful email sent
#' @import httr
#' @export
sendEmail <- function(email = "cp84@uw.edu",
                      mail_message = "gas prices have NAs") {
  url <-
    "https://api.mailgun.net/v3/sandbox4b565673c15246d8b4f6f15176a7e3fc.mailgun.org/messages"
  ## username:password so api_key is all after the api:
  api_key <- Sys.getenv("MAILGUN_API_KEY")
  the_body <-
    list(
      from = "Mailgun Sandbox <postmaster@sandbox4b565673c15246d8b4f6f15176a7e3fc.mailgun.org>",
      to = email,
      subject = "gas prices have NAs",
      text = mail_message
    )

  req <- httr::POST(url,
                    httr::authenticate("api", api_key),
                    encode = "form",
                    body = the_body)

  httr::stop_for_status(req)

  TRUE
}

#' get gas prices from gas buddy API
#'
#' Run tripgen::update_wa_bevs() to update the BEVs in the state.
#'
#' @return TRUE if update was successful
#' @import httr
#'

get_gas_prices <- function(zip) {
  Sys.sleep(0.5)
  url  <- "https://www.gasbuddy.com/assets-v2/api/stations?fuel=1&search="
  # browser()
  url_zip <- paste0(url, zip)
  #print(url_zip)
  # Get raw data from the website
  raw.result <- GET(url = url_zip)
  # Convert to char
  this.raw.content <- rawToChar(raw.result$content)
  # Convert to JSON
  this.content <- jsonlite::fromJSON(this.raw.content)
  # Extract the average gas price for today for the Zip
  avg_gas_price <- this.content$trends$Today[1]

  return(avg_gas_price)
}

#' Update the gas prices in the database by fetching the latest from gas buddy and overwriting
#'
#' Run tripgen::update_wa_bevs() to update the BEVs in the state.
#'
#' @return TRUE if update was successful
#' @import httr
#' @export
update_wa_gas_prices <- function() {
  # Initialization ------------------

  devtools::load_all("./R/setup_logging.R")
  ## Setup the logging destination
  lg <-
    lgr::get_logger("test")$set_propagate(FALSE)$set_appenders(lgr::AppenderJson$new(layout = LayoutLogstash$new(), file = here::here(
      paste0(
        "logs/update_wa_gas_prices",
        as.character(Sys.Date()),
        ".log"
      )
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

  # data(zipcode)
  WA_zips <- zipcode$zip[which(zipcode$state == "WA")]
  # browser()
  avg_gas_price <- lapply(WA_zips, get_gas_prices)
  # browser()
  # replace the missing values with today's avg price
  list_gas_prices <- tidyr::replace_na(avg_gas_price, 2.79)
  gas_prices <-
    data.frame(zip = WA_zips, avg_gas_price = unlist(list_gas_prices))

  # Something wrong with zip 98363 - Port Angeles, so setting it manually
  # gas_prices[which(gas_prices$zip == 98363), ]$avg_gas_price <-
  #   3.24

  # Send an email if some rows are incomplete, i.e. logic above fails
  gas_prices_incomp <- gas_prices[!complete.cases(gas_prices),]
  if (nrow(gas_prices_incomp) > 0) {
    sendEmail(mail_message = toString(gas_prices_incomp))
  }
  # browser()
  gas_prices_comp <- gas_prices[complete.cases(gas_prices),]

  # Execute as transaction
  # DBI::dbBegin(main_con)
  # DBI::dbWriteTable(main_con, "wa_gas_prices", gas_prices_comp, overwrite = TRUE)
  #
  # res <-
  #   main_con %>% DBI::dbSendQuery(
  #     paste0(
  #       "insert into table_stats (table_name, last_updated) values ('wa_gas_prices', '",
  #       as.character(Sys.time()),
  #       "') on conflict (table_name) do update set last_updated = EXCLUDED.last_updated;"
  #     )
  #   )
  # DBI::dbClearResult(res)
  # DBI::dbCommit(main_con)
  # DBI::dbDisconnect(main_con)
  return(gas_prices_comp)
}
