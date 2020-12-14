#' Generate random date times between two date-times, for a timezone
#' Adapted from : https://stackoverflow.com/a/14721124/1328232
#'
#' @param N Number of datetime values to generate
#' @param st Start date in the format "yyyy/mm/dd"
#' @param et End date in the format "yyyy/mm/dd"
#' @param tz Timezone for the dates
#' @param config constants
#'
#' @export
#'
#' @return The randomly generated date time.
#'
rnd_date_time <-
  function(N,
           st = "2019/07/01",
           et = "2019/07/02",
           tz = "America/Los_Angeles",
           config) {
    st <- as.POSIXct(st, tz = tz)
    et <- as.POSIXct(et, tz = tz)
    dt <- as.numeric(difftime(et, st, units = "secs"))
    set.seed(config[['GLOBAL_SEED']])
    ev <- sort(stats::runif(N, 0, dt))
    rt <-
      lubridate::as_datetime(st + ev, tz = "America/Los_Angeles")

    return(rt)
  }

#' Predicts the probability of choice of ICEVs vs Rental vehicle vs an EV for a trip
#' Taken from Yan's paper
#'
#' @param ev_range - range of the EV chosen for the trip
#' @param trip_row Dataframe that gives trip relates parameters -
#'                       dist - trip distance
#'                       dest_charger_L2 - whether trip destination has a Level-2 Charger
#'                       dest_charger - whether destination charger has a DCFC
#'                       max_spacing - Maximum gap between DCFC chargers along the shortest path for the trip
#'                       gas_price - Price of gas at the origin zip code of the trip
#' @param config constants
#'
#' @export
#'
#' @return The probability of travel by EV

vcdm_scdm4 <- function(ev_range, trip_row, config) {
  # Input Validation --------------------------------------------------------

  if (nrow(trip_row) == 0) {
    stop('trip_row has 0 rows - should have just 1')
  }
  else if (nrow(trip_row) > 1) {
    stop('trip_row has more than 1 rows - should have just 1')
  }
  if (is.null(config[['AVG_FUEL_ECONOMY_OWN']])) {
    stop('config is missing the field `AVG_FUEL_ECONOMY_OWN`')
  }
  if (is.null(config[['AVG_FUEL_ECONOMY_RENTAL']])) {
    stop('config is missing the field `AVG_FUEL_ECONOMY_RENTAL`')
  }
  if (is.null(config[['AVG_RESTROOM_SPACING']])) {
    stop('config is missing the field `AVG_RESTROOM_SPACING`')
  }
  if (is.null(config[['AVG_RENTAL_CAR_COST']])) {
    stop('config is missing the field `AVG_RENTAL_CAR_COST`')
  }
  if (!(class(ev_range) == 'numeric' ||
        class(ev_range) == 'integer')) {
    stop('ev_range is of non-numeric type - provide numeric value for ev_range')
  }
  if (ev_range <= 0) {
    stop('ev_range should be a positive value')
  }
  # browser()
  # VCDM Parameters ---------------------------------------------------------

  theta_1 <- -0.04
  theta_2 <- 0.059
  theta_3 <- -0.075
  theta_4 <- -1.659 # (l / R_full)
  theta_5 <- -9.342 # (Max_spacing / R_full)
  theta_6 <- 0.002
  theta_7 <- 0.197
  theta_8 <- -0.748
  theta_9 <-  1.428 # (destination_charger Level_3)
  ASC_BEV <- 11.184
  ev_range <-
    ev_range * config$SOC_LOWER_LIMIT / 100 # This is as some EVs can start with the lower SOC value
  # Ideally change the above to min(SOC_LOWER_LIMIT, MAX_SOC) as after fast charge the vehicle may only go up-to 80% (MAX_SOC)

  # Probability calculation ------------------------------------------------

  util_ij_ice <-
    theta_1 * trip_row$gas_price * trip_row$dist / config$AVG_FUEL_ECONOMY_OWN

  # Utility of driving a rental ICE
  util_ij_rent <-
    (theta_2 * config$AVG_RENTAL_CAR_COST) + (theta_3 * trip_row$gas_price * trip_row$dist / config$AVG_FUEL_ECONOMY_RENTAL)

  # utility of driving a BEV for the trip
  util_ij_bev <-
    ((theta_4 * (trip_row$dist / ev_range)) + (theta_5 * trip_row$max_spacing / ev_range) + (theta_6 * config$AVG_RESTROOM_SPACING) + theta_7 * 1 +
       (theta_8 * trip_row$dest_charger_L2) +
       (theta_9 * trip_row$dest_charger) + ASC_BEV
    )

  # Find the odds from utility
  # odds_ij <- exp(util_ij)
  # Find the probability from odds
  prob_ij_bev <-
    exp(util_ij_bev) / (exp(util_ij_bev) + exp(util_ij_rent) + exp(util_ij_ice))

  # print(prob_ij_bev)
  return(prob_ij_bev)
}

#' Create a dataframe that predicts the number of trips returning from origin to destination
#'
#' @param od A dataframe containing the results of the gravity model
#' @param od_sp Dataframe containing all trip rows
#' @param config constants
#'
#' @return A dataframe with column origin, destination, and
#' number of trips between the origin and destination
#'
#' @export
#'
#' @import magrittr
#' @importFrom rlang .data
#'
create_return_df <- function(od, od_sp, config) {
  # Input Validation --------------------------------------------------------
  # browser()
  if (dim(od)[1] == 0) {
    stop('od has 0 rows - should have atleast 1')
  }
  if (!('origin' %in% colnames(od))) {
    stop('od is missing the necessary column origin')
  }
  if (!('destination' %in% colnames(od))) {
    stop('od is missing the necessary column destination')
  }
  if (!('ret_calib_daily' %in% colnames(od))) {
    stop('od is missing the necessary column ret_calib_daily')
  }
  if (!('devs' %in% colnames(od))) {
    stop('od is missing the necessary column devs')
  }
  if (!('dcars' %in% colnames(od))) {
    stop('od is missing the necessary column dcars')
  }
  if (!('origin' %in% colnames(od_sp))) {
    stop('od_sp is missing the necessary column origin')
  }
  if (!('destination' %in% colnames(od_sp))) {
    stop('od_sp is missing the necessary column destination')
  }
  if (!('shortest_path_length' %in% colnames(od_sp))) {
    stop('od_sp is missing the necessary column shortest_path_length')
  }
  if (is.null(config[['CRITICAL_DISTANCE']])) {
    stop('config is missing the field `CRITICAL_DISTANCE`')
  }
  if (is.null(config[['GLOBAL_SEED']])) {
    stop('config is missing the field `GLOBAL_SEED`')
  }
  if (!(class(config[['CRITICAL_DISTANCE']]) == 'numeric' ||
        class(config[['CRITICAL_DISTANCE']]) == 'integer')) {
    stop('`CRITICAL_DISTANCE` should be of class numeric or integer')
  }
  if (!(class(config[['GLOBAL_SEED']]) == 'numeric' ||
        class(config[['GLOBAL_SEED']]) == 'integer')) {
    stop('`GLOBAL_SEED` should be of class numeric or integer')
  }
  if (config[['CRITICAL_DISTANCE']] < 0) {
    stop('`CRITICAL_DISTANCE` should be a positive number')
  }
  if (config[['GLOBAL_SEED']] < 0) {
    stop('`GLOBAL_SEED` should be a positive number')
  }

  # Assignment --------------------------------------------------------------
  set.seed(config[['GLOBAL_SEED']])
  # random draw from Poisson distribution
  daily_counts_ret <-
    stats::rpois(dim(od)[1], od$ret_calib_daily)

  set.seed(config[['GLOBAL_SEED']])
  # Draw from a binomial distro
  evtrips_ret <-
    mapply(stats::rbinom, 1, daily_counts_ret, od$devs / od$dcars)

  return <-
    cbind(od$origin, od$destination, evtrips_ret) #add origin and destination zip code to the data

  return_df <- as.data.frame(return)

  names(return_df) <-
    c("origin",
      "destination",
      "long_distance_return_trips")

  # Merge the return & departure df with all_trips_sp_df to get the distances for OD pairs
  return_distances <-
    base::merge(
      return_df,
      od_sp,
      by.x = c('origin', 'destination'),
      by.y = c('origin', 'destination')
    )

  # Filter out OD pairs that are less than 70 miles apart
  return_distances_CD <-
    return_distances %>% dplyr::filter(.data$shortest_path_length >= config$CRITICAL_DISTANCE)

  # Filter out OD pairs with non-zero trips
  nz_return <-
    return_distances_CD %>% dplyr::filter(.data$long_distance_return_trips > 0)

  # print(nz_return)
  return(nz_return)
}

#' Create a dataframe that predicts the number of trips departing from origin to destination
#'
#' @param od A dataframe containing the results of the gravity model
#' @param od_sp Dataframe containing OD and shortest_path_length
#' @param config constants
#'
#' @return A dataframe with column origin, destination, and
#' number of trips between the origin and destination
#'
#' @export
#'
#' @import magrittr
#' @importFrom rlang .data
#'
create_departure_df <- function(od, od_sp, config) {
  # Input Validation --------------------------------------------------------

  if (dim(od)[1] == 0) {
    stop('od has 0 rows - should have atleast 1')
  }
  if (!('origin' %in% colnames(od))) {
    stop('od is missing the necessary column origin')
  }
  if (!('destination' %in% colnames(od))) {
    stop('od is missing the necessary column destination')
  }
  if (!('dep_calib_daily' %in% colnames(od))) {
    stop('od is missing the necessary column dep_calib_daily')
  }
  if (!('oevs' %in% colnames(od))) {
    stop('od is missing the necessary column oevs')
  }
  if (!('ocars' %in% colnames(od))) {
    stop('od is missing the necessary column ocars')
  }
  if (!('origin' %in% colnames(od_sp))) {
    stop('od_sp is missing the necessary column origin')
  }
  if (!('destination' %in% colnames(od_sp))) {
    stop('od_sp is missing the necessary column destination')
  }
  if (!('shortest_path_length' %in% colnames(od_sp))) {
    stop('od_sp is missing the necessary column shortest_path_length')
  }
  if (is.null(config[['CRITICAL_DISTANCE']])) {
    stop('config is missing the field `CRITICAL_DISTANCE`')
  }
  if (!(class(config[['CRITICAL_DISTANCE']]) == 'numeric' ||
        class(config[['CRITICAL_DISTANCE']]) == 'integer')) {
    stop('`CRITICAL_DISTANCE` should be of class numeric')
  }
  if (config[['CRITICAL_DISTANCE']] < 0) {
    stop('`CRITICAL_DISTANCE` should be a positive number')
  }

  # Assignment -----------------------------

  set.seed(config[['GLOBAL_SEED']])

  daily_counts_dep <-
    stats::rpois(dim(od)[1], od$dep_calib_daily)   #random draw from Poisson distribution

  set.seed(config[['GLOBAL_SEED']])
  # Draw from a binomial distro
  evtrips_dep <-
    mapply(stats::rbinom, 1, daily_counts_dep, od$oevs / od$ocars)

  departure <-
    cbind(od$origin, od$destination, evtrips_dep) #add origin and destination zip code to the data

  departure_df <- as.data.frame(departure)

  names(departure_df) <-
    c("origin",
      "destination",
      "long_distance_departure_trips")

  departure_distances <-
    base::merge(
      departure_df,
      od_sp,
      by.x = c('origin', 'destination'),
      by.y = c('origin', 'destination')
    )
  departure_distances_CD <-
    departure_distances %>% dplyr::filter(.data$shortest_path_length >= config$CRITICAL_DISTANCE)

  nz_departure <-
    departure_distances_CD %>% dplyr::filter(.data$long_distance_departure_trips > 0)
  # print(nz_departure)
  return(nz_departure)
}

#' Get the trips EVs from the relevant EVs belonging to a source zip code
#'
#'@param trips_source_i Number of trips from a source zip code
#'@param source_EVs the dataframe of EVs from the zip code
#'
#' @export
#'
#'@return The selected trip EVs from the set of EVs at source
#'
get_tripEVs_from_sourceEVs <-
  function(trips_source_i, source_EVs, config) {
    # Input validation --------------------------------------------------------

    if (!class(source_EVs) == 'data.frame') {
      stop('source_EVs should be of type data.frame')
    }
    if (!(class(trips_source_i) == 'numeric' ||
          class(trips_source_i) == 'integer')) {
      stop('trips_source_i should be an integer or numeric value')
    }
    if (trips_source_i < 0) {
      stop('trips_source_i should be a positive integer')
    }

    # Assignment --------------------------------------------------------------

    # An EV cannot make two trips in a day
    if (trips_source_i > nrow(source_EVs)) {
      trips_source_i <- nrow(source_EVs)

    }

    set.seed(config[['GLOBAL_SEED']])

    # Randomly pick EVs from the relevant EVs "without replacement"
    trip_EVs <-
      dplyr::sample_n(source_EVs, trips_source_i, replace = FALSE)

    return(trip_EVs)
  }


#' This function returns the gas price for the zip code from the gas price dataframe
#'
#' @param gas_prices The dataframe containing gas price for each zip code
#' @param origin_zip The zip for which gas price is to be known
#'
#' @export
#'
#' @return The gas price for the relevant zip code
#'
get_Gas_Price <- function(gas_prices, origin_zip) {
  # Input validation -------------------------------------------------------

  if (!class(gas_prices) == 'data.frame') {
    stop('gas_prices should be of type data.frame')
  }
  if (!(class(origin_zip) == 'numeric' ||
        class(origin_zip) == 'integer')) {
    stop('origin_zip should be an integer or numeric value')
  }
  if (origin_zip < 0) {
    stop('origin_zip should be a positive integer')
  }

  # Gas price lookup --------------------------------------------------------

  gas_price <-
    gas_prices$avg_gas_price[gas_prices$zip == origin_zip]

  gas_price <-
    ifelse(length(gas_price) == 0,
           3.0,
           gas_price)

  return(gas_price)
}
#'
#' This function creates a dataframe with trip details needed for the VCDM.
#'
#' @param main_con The database connection passed to another function
#' @param a_id The analysis_id
#' @param gas_prices The dataframe containing gas prices for each zip code
#' @param trip_EV_row The dataframe containing details about the EV taking the trip
#' @param trip_sp Dataframe containing trip shortest_path
#' @param trip_dc Dataframe with trip destination chargers
#'
#' @export
#'
#' @return The dataframe with EV specific trip details
#'
make_trip_row <-
  function(main_con,
           a_id = 1,
           gas_prices,
           trip_EV_row,
           trip_sp,
           trip_dc) {
    # Input validation --------------------------------------------------------

    if (!class(gas_prices) == 'data.frame') {
      stop('gas_prices should be of type data.frame')
    }
    if (!class(trip_EV_row) == 'data.frame') {
      stop('trip_EV_row should be of type data.frame')
    }
    if (!class(trip_sp) == 'data.frame') {
      stop('trip_sp should be of type data.frame')
    }
    if (!class(trip_dc) == 'data.frame') {
      stop('trip_dc should be of type data.frame')
    }
    if (!('connector_code' %in% colnames(trip_EV_row))) {
      stop('trip_EV_row should have a column connector_code')
    }
    if (!(
      class(trip_EV_row$connector_code) == 'integer' ||
      class(trip_EV_row$connector_code) == 'numeric'
    )) {
      stop('column connector_code in trip_EV_row should be of type integer')
    }
    if (!('origin_zip' %in% colnames(trip_EV_row))) {
      stop('trip_EV_row should have a column origin_zip')
    }
    if (!(
      class(trip_EV_row$origin_zip) == 'integer' ||
      class(trip_EV_row$origin_zip) == 'numeric'
    )) {
      stop('column origin_zip in trip_EV_row should be of type integer')
    }

    # Input correction --------------------------------------------------------

    # trip_cd is null, then give it path length
    # if (rapportools::is.empty(trip_cd$cd_chademo)) {
    #   # lg$fatal(msg = paste("cd_chademo is null"),
    #   #          trip_EV_row = trip_EV_row)
    #   # In case the dataframe is empty create a new dataframe and add columns
    #   # trip_cd <- data.frame()
    #   # browser()
    #   trip_cd$cd_chademo <- numeric(nrow(trip_cd))
    #   trip_cd[1, 'cd_chademo'] <-
    #     trip_sp$shortest_path_length[1]
    # }
    # if (rapportools::is.empty(trip_cd$cd_combo)) {
    #   # lg$fatal(msg = paste("cd_combo is null"),
    #   #          trip_EV_row = trip_EV_row)
    #   #  browser()
    #   trip_cd$cd_combo <- numeric(nrow(trip_cd))
    #   trip_cd[1, 'cd_combo'] <-
    #     trip_sp$shortest_path_length[1]
    # }

    # Assignment --------------------------------------------------------------

    if (trip_EV_row$connector_code == 1) {
      #      max_spacing <- trip_cd$cd_chademo # in miles
      dest_charger <-
        trip_dc$dc_chademo
    } else if (trip_EV_row$connector_code == 2) {
      #     max_spacing <- trip_cd$cd_combo
      dest_charger <-
        trip_dc$dc_combo
    } else {
      # Need to replace this with Tesla Superchargers
      # max_spacing <-
      #   base::min(trip_cd$cd_chademo ,
      #             trip_cd$cd_combo)
      dest_charger <-
        trip_dc$dc_chademo |
        trip_dc$dc_combo
    }

    dest_charger_L2 <-
      ifelse(is.na(trip_dc$dc_level2),
             0,
             trip_dc$dc_level2)
    dest_charger <-
      ifelse(is.na(dest_charger), 0, dest_charger)

    gas_price <-
      get_Gas_Price(gas_prices = gas_prices, origin_zip = trip_EV_row$origin_zip)

    max_spacing <- get_max_spacing(
      main_con = main_con,
      a_id = a_id,
      origin = trip_EV_row$origin_zip,
      dest = trip_EV_row$destination_zip,
      connector_code = trip_EV_row$connector_code
    )
    trip_row <- data.frame(
      dist = trip_sp$shortest_path_length,
      dest_charger_L2 = dest_charger_L2,
      dest_charger = dest_charger,
      max_spacing = max_spacing,
      gas_price = gas_price
    )

    return(trip_row)
  }

#' Get max spacing between charging stations
#'
#' @param main_con The database connection
#' @param a_id The analysis_id
#' @param origin The origin zip code
#' @param destination The destination zip code
#' @param connector_code The connector_code for the EV - ChaDEMO or COMBO
#'
#' @export
#'
#' @return max_spacing The maximum spacing (in miles) between charging stations along the shortest path
#'
get_max_spacing <-
  function(main_con,
           a_id,
           origin,
           dest,
           connector_code) {
    # Input validation --------------------------------------------------------

    if (!(class(origin) == 'integer' ||
          class(origin) == 'numeric')) {
      stop('origin should be of type integer or numeric')
    }
    if (!(class(dest) == 'integer' || class(dest) == 'numeric')) {
      stop('destination should be of type integer or numeric')
    }
    if (!is.element(connector_code, c(1, 2, 4))) {
      stop('connector_code should be either 1, 2 or 4')
    }

    # For non-tesla vehicles
    if (connector_code != 4) {
      query_msp <-
        glue::glue(
          "select (max(delr) * st_length(line::geography) * 0.000621371) as max_spacing
from
    (select sq2.ratio - coalesce((lag(sq2.ratio) over (
                                                       order by sq2.ratio)), 0) as delr,
            line
     from
         (select ST_LineLocatePoint(line, sqa.points) as ratio,
                 line
          from sp_od2({origin}, {dest}) as line,

              (select st_setsrid(st_makepoint(longitude, latitude), 4326) as points
               from
                   (select longitude,
                           latitude
                    from evses_now
                    where analysis_id = {a_id} and connector_code = {connector_code}
                        or connector_code = 3
                    union select longitude,
                                 latitude
                    from zipcode_record
                    where zip = '{origin}'
                        or zip = '{dest}') as sq) as sqa
          where st_dwithin(line::geography, sqa.points::geography, 16093.4)
          order by ratio asc) as sq2) as sq3
group by sq3.line;"
        )
    } else {
      # all chargers are game, the where clause vanishes in the evses_now table
      query_msp <-
        glue::glue(
          "select (max(delr) * st_length(line::geography) * 0.000621371) as max_spacing  from (
select sq2.ratio - coalesce((lag(sq2.ratio) over (order by sq2.ratio)), 0) as delr, line from
(select ST_LineLocatePoint(line, sq.points) as ratio, line from
sp_od2({origin}, {dest}) as line, (select st_setsrid(st_makepoint(longitude, latitude), 4326) as points from evses_now where analysis_id = {a_id}) as sq
where st_dwithin(line::geography, sq.points::geography, 16093.4)
order by ratio asc) as sq2) as sq3
group by sq3.line;"
        )
    }

    max_spc_df <- DBI::dbGetQuery(main_con, query_msp)

    if (rapportools::is.empty(max_spc_df$max_spacing)) {
      # there were no charging stations in vicinity of the road
      # so the max spacing is the length of the shortest path
      query_sp <-
        glue::glue(
          "select (st_length(line::geography) * 0.000621371) as splen from (
select sp_od2({origin}, {dest}) as line ) as sq"
        )

      spl_df <- DBI::dbGetQuery(main_con, query_sp)
      max_spacing <- spl_df$splen

    } else {
      max_spacing <- max_spc_df$max_spacing
    }

    return(max_spacing)

  }

#' Make EVSES_NOW table
#'
#' This function `make_evses_now_table()` creates a database table which contains
#' the evses for this analysis. This is a combination of existing EVSES and proposed EVSES
#'
#'
#' @param main_con The database connection to use to interact with the database
#' @param a_id analysis_id
#'
#' @return evses_now dataframe
#'
#' @export
#'
#' @import magrittr
#' @importFrom utils data
#' @importFrom rlang .data
#'
make_evses_now_table <- function(main_con, a_id = 1) {
  # Get all new EVSEs for the said analysis_id where we have some fast charger
  query_nevses <-
    paste0(
      "select * from new_evses where dcfc_plug_count > 0 and in_service = 'true' and analysis_id = ",
      a_id
    )

  nevses <- DBI::dbGetQuery(main_con, query_nevses)

  # Get all the built evses
  query_bevses <-
    "select * from built_evse where in_service = 'true'"

  bevses <- DBI::dbGetQuery(main_con, query_bevses)
  # longitude, latitude, connector_code, dcfc_plug_count, dcfc_fixed_charging_price, dcfc_var_charging_price_unit, dcfc_var_charging_price, dcfc_fixed_parking_price, dcfc_var_parking_price_unit, dcfc_var_parking_price
  # Select the necessary columns and rows with DCFC, as we are collecting Level2 as well
  bevses <-
    bevses[, c(
      "bevse_id",
      "longitude",
      "latitude",
      "connector_code",
      "dcfc_count",
      "dcfc_fixed_charging_price",
      "dcfc_var_charging_price_unit",
      "dcfc_var_charging_price",
      "dcfc_fixed_parking_price",
      "dcfc_var_parking_price_unit",
      "dcfc_var_parking_price"
    )] %>%
    dplyr::filter(.data$dcfc_count >= 1) %>%
    dplyr::filter(.data$connector_code == 1 |
                    .data$connector_code == 2 |
                    .data$connector_code == 3)

  # bevses$dcfc_count <- NULL

  # Join the two dataframes to create the EVSES now
  evses_now <-
    bevses %>% dplyr::mutate(evse_id = paste0("b", .data$bevse_id))
  evses_now$bevse_id <- NULL

  nevses <-
    nevses[, c(
      "nevse_id",
      "latitude",
      "longitude",
      "dcfc_plug_count",
      "connector_code",
      "dcfc_fixed_charging_price",
      "dcfc_var_charging_price_unit",
      "dcfc_var_charging_price",
      "dcfc_fixed_parking_price",
      "dcfc_var_parking_price_unit",
      "dcfc_var_parking_price"
    )] %>%
    dplyr::mutate(evse_id = paste0("n", .data$nevse_id)) %>%
    dplyr::rename(dcfc_count = dcfc_plug_count)

  nevses$nevse_id <- NULL

  evses_now <- rbind(evses_now, nevses)
  evses_now$analysis_id <- a_id

  # Create a table with total evses - in the evses schema
  DBI::dbAppendTable(main_con, "evses_now", evses_now)
  # Make a unique table for each analysis_id and drop it after the analysis is complete

  return(evses_now)
}


#' Create the EV fleet for ABM simulation based on SDCM VCDM
#'
#' This function `trip_gen()` creates a fleet of EVs for EVI-ABM simulation
#' using the published models or vehicle choice using static choice discrete models.
#'
#'
#' @param num_days Number of days to generate the EV fleet for.
#' @param config constants
#' @param a_id analysis_id
#'
#' @return Day EVs
#'
#' @export
#'
#' @import magrittr
#' @importFrom utils data
#' @importFrom rlang .data
#'
trip_gen <- function(num_days = 1,
                     config,
                     a_id = 1) {
  # Initialization ------------------

  source("./R/setup_logging.R")
  ## Setup the logging destination
  lg <-
    lgr::get_logger("test")$set_propagate(FALSE)$set_appenders(lgr::AppenderJson$new(layout = LayoutLogstash$new(), file = here::here(
      paste0("logs/runner_", as.character(a_id), ".log")
    )))

  if (is.na(a_id)) {
    print("missing indeed")
    lg$log(level = "fatal",
           msg = "Analysis_id not passed in trip gen calc",
           "ip" = ipify::get_ip())
    a_id = 1
    # a_id  <-  99
  }

  print(config)

  # Database settings -------------------------------------------------------

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

  main_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("MAIN_HOST"),
    dbname = Sys.getenv("MAIN_DB"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"),
    port = Sys.getenv("MAIN_PORT")
  )


  # browser()
  # Select queries ----------------------------------------------------------

  # gas prices in WA
  wa_gas_prices <-
    DBI::dbGetQuery(main_con, 'select * from wa_gas_prices')

  wa_bevs <-
    DBI::dbGetQuery(main_con,
                    "select veh_id, range_fe, zip_code, connector_code, make from wa_bevs")

  # These are the results of the EV trips generation from PJ
  wa_evtrips <-
    DBI::dbGetQuery(
      main_con,
      "select wae.origin,
       wae.destination,
       wae.ocars,
       wae.dcars,
       wae.ret,
       wae.dep,
       coalesce(wabo.count, 0)   as oevs,
       coalesce(wabd.count, 0)   as devs,
       coalesce(wabost.count, 0) as oevs_no_tesla,
       coalesce(wabdst.count, 0) as devs_no_tesla
from wa_evtrips wae
         left join (select count(veh_id), zip_code from wa_bevs group by zip_code) wabo
                   on wabo.zip_code = wae.origin
         left join (select count(veh_id), zip_code from wa_bevs group by zip_code) wabd
                   on wabd.zip_code = wae.destination
         left join (select count(veh_id), zip_code from wa_bevs where lower(make) <> 'tesla' group by zip_code) wabost
                   on wabost.zip_code = wae.origin
         left join (select count(veh_id), zip_code from wa_bevs where lower(make) <> 'tesla' group by zip_code) wabdst
                   on wabdst.zip_code = wae.destination;"
    )
  # browser()
  od_sp <-
    DBI::dbGetQuery(main_con, 'select * from od_sp')

  od_cd <-
    DBI::dbGetQuery(
      main_con,
      paste0(
        'select origin, destination, min(cd_chademo) as cd_chademo, min(cd_combo) as cd_combo from od_cd where analysis_id = -1 or analysis_id =  ',
        a_id,
        ' group by origin, destination;'
      )
    )

  dest_charger <- DBI::dbGetQuery(
    main_con,
    paste0(
      'select zip, bool_or(dc_chademo) as dc_chademo, bool_or(dc_combo) as dc_combo, bool_or(dc_level2) as dc_level2 from dest_charger where analysis_id = -1 or analysis_id =  ',
      a_id,
      ' group by zip;'
    )
  )

  include_tesla_flag <-
    DBI::dbGetQuery(
      main_con,
      paste0(
        "select include_tesla from analysis_record where analysis_id = ",
        a_id
      )
    )$include_tesla

  # Check if evses_now table exists
  evses_now_exists <- DBI::dbGetQuery(
    main_con,
    glue::glue(
      'select exists(select 1 from evses_now where analysis_id = {a_id}) AS "exists";'
    )
  )$exists

  if (evses_now_exists) {
    evses_now <-
      DBI::dbGetQuery(main_con, paste0("select * from evses_now where analysis_id = ", a_id))
  } else {
    evses_now <-
      make_evses_now_table(a_id = a_id, main_con = main_con)
  }

  # Check table data -------------------------

  if ((exists("wa_gas_prices")) &&
      (nrow(wa_gas_prices) > 0) &&
      (exists("wa_bevs")) &&
      (nrow(wa_bevs) > 0) &&
      (exists("wa_evtrips")) &&
      (nrow(wa_evtrips) > 0) &&
      (exists("od_sp")) &&
      (nrow(od_sp) > 0) &&
      (exists("dest_charger")) && (nrow(dest_charger) > 0) &&
      (exists("evses_now")) && (nrow(evses_now) > 0)) {
    lg$log(level = "info",
           msg = "Data tables seem exist and well-populated",
           "ip" = ipify::get_ip())
  } else {
    lg$log(level = "fatal",
           msg = "Some data tables missing or incomplete",
           "ip" = ipify::get_ip())
    stop("Some data tables missing or incomplete")
  }

  # Processing begins------------------------

  if (!include_tesla_flag) {
    print("No teslas in the simulation")
    lg$log(level = "info",
           msg = "No teslas in simulation",
           "ip" = ipify::get_ip())

    # Remove Tesla vehicles --------------

    wa_evtrips$oevs  <- NULL
    wa_evtrips$devs  <- NULL
    wa_evtrips <-
      wa_evtrips %>% dplyr::rename(oevs = oevs_no_tesla, devs = devs_no_tesla)
    wa_bevs <-
      wa_bevs %>% dplyr::filter(!grepl("tesla", make, ignore.case = TRUE)) # ~ 16000
  }

  # 1. Calculate trip generation rate -----------

  wa_evtrips$dep_calib_daily <-  wa_evtrips$dep * 354.3496 / 30
  wa_evtrips$ret_calib_daily <-  wa_evtrips$ret * 354.3496 / 30

  # Make all NA distances to zero, since we wont be able to traverse on them avayway
  # EVentually we filter all OD pairs where the distance is less than a threshold
  od_sp$shortest_path_length[which(is.na(od_sp$shortest_path_length))] <-
    0

  returning_counter <- 0
  departing_counter <- 0

  for (simi in 1:num_days) {
    # update this based on how many iterations of trip data generation you need

    #print(" Finding the returning and departing EV trips for the day ")

    # 2. Calculate trips from trip generation rates -----------------
    nz_return <-
      create_return_df(od =  wa_evtrips,
                       od_sp =  od_sp,
                       config = config)  # Typically ~ 700
    lg$log(
      level = "info",
      msg = paste0("Return trips calculated, count: ", nrow(nz_return)),
      "ip" = ipify::get_ip()
    )
    nz_departure <-
      create_departure_df(od =  wa_evtrips,
                          od_sp =  od_sp,
                          config = config) # ~ 500
    lg$log(
      level = "info",
      msg = paste0("Departure trips calculated, count: ", nrow(nz_departure)),
      "ip" = ipify::get_ip()
    )
    # Find corresponding EV to make the trip
    return_EVs <- data.frame()
    departure_EVs <- data.frame()

    # Get the simulation day from SIM_DATES
    sim_day <- config$SIM_START_DATE#SIM_DATES[simi]

    # 3. Group-by final trips based the EV source --------------
    # destination for returning trips and origin for departing trips
    nz_return_g <-
      nz_return %>% dplyr::group_by(.data$destination) %>% dplyr::summarise(n = base::sum(.data$long_distance_return_trips))
    nz_departure_g <-
      nz_departure %>% dplyr::group_by(.data$origin) %>% dplyr::summarise(n = base::sum(.data$long_distance_departure_trips))

    # 4. Merge the two dfs to get the total EV requirement per day -----------
    EV_req <-
      base::merge(
        nz_return_g,
        nz_departure_g,
        by.x = "destination",
        by.y = "origin",
        all = TRUE
      ) # ~ 230 zipcodes due to groupby
    # Fill NAs with 0
    data.table::setnafill(EV_req, fill =  0)

    # 5. Find total trips from a source ------------------------
    # and rename the column "destination" to "source"
    EV_req_tots <-
      EV_req %>% dplyr::mutate(total_trips = .data$n.x + .data$n.y) %>% dplyr::rename(
        source = .data$destination,
        returning_trips = .data$n.x,
        departing_trips = .data$n.y
      )

    lg$log(
      level = "info",
      msg = paste0("Departure trips calculated, count: ", nrow(nz_departure)),
      "ip" = ipify::get_ip()
    )
    # browser()
    lg$log(
      level = "info",
      msg = paste("EV_req_tots with rows: ", nrow(EV_req_tots)),
      "EV_req_tots" = EV_req_tots,
      "ip" = ipify::get_ip()
    )

    # 6. Make random draws from the EVs at the source --------------------
    # for completing these many trips
    # These EVs will then be distributed into departing and returning trips
    i = 1
    for (i in 1:nrow(EV_req_tots)) {
      # Find corresponding EV to make the trip
      return_EVs <- data.frame()
      departure_EVs <- data.frame()

      trips_source_i <- EV_req_tots$total_trips[i]

      # 7. Filter the EVs in the source zip code
      # that could have made the trip
      source_EVs <-
        wa_bevs %>% dplyr::filter(.data$zip_code == EV_req_tots$source[i])

      # If EVs are available in this zip code
      if (nrow(source_EVs) > 0) {
        # 8. Get trips EVs from source EVs ------------------
        trip_EVs <-
          get_tripEVs_from_sourceEVs(
            trips_source_i = trips_source_i,
            source_EVs = source_EVs,
            config = config
          )
        # print("Generated a list of EVs for the days trips - now separate into return and departure")

        if (dim(trip_EVs)[1] == 0) {
          # lg$trace(msg = paste("trip_EVs has no rows just schema i = ", i))
        }

        dst <-
          lubridate::as_datetime(paste(sim_day, config$START_TIME), tz = "America/Los_Angeles")
        det <-
          lubridate::as_datetime(paste(sim_day, config$END_TIME), tz = "America/Los_Angeles")

        # 9. Process returning vehicles ----------------------------

        # 9.1 Iterate through an OD pair ---------------
        trip_EVs_returning <- data.frame()
        if (EV_req_tots$returning_trips[i] > 0) {
          returning_counter <- 1
          nz_return_source <-
            nz_return[nz_return$destination == EV_req_tots$source[i],]
          j <- 1
          for (j in 1:nrow(nz_return_source)) {
            # Find the number of trips between the OD pair
            trip_count_OD <-
              nz_return_source$long_distance_return_trips[j]

            if ((trip_count_OD > 0) &&
                (nrow(trip_EVs) >= returning_counter + trip_count_OD - 1)) {
              trip_EVs_returning_OD <-
                dplyr::slice(trip_EVs,
                             returning_counter:(returning_counter + trip_count_OD - 1))
              # Increment the counter, so we select new EVs from trip_EVs
              returning_counter <-
                returning_counter + trip_count_OD

              if (dim(trip_EVs_returning_OD)[1] > 0) {
                trip_EVs_returning_OD$origin_zip <- nz_return_source$origin[j]
                trip_EVs_returning_OD$destination_zip <-
                  nz_return_source$destination[j]

                # 9.2 Randomly assign SOCs to these vehicles -----
                #      with replacement
                set.seed(config[['GLOBAL_SEED']])
                trip_EVs_returning_OD$soc <-
                  base::sample(
                    config$SOC_LOWER_LIMIT:config$SOC_UPPER_LIMIT,
                    trip_count_OD,
                    replace = TRUE
                  )

                trip_EVs_returning <-
                  rbind(trip_EVs_returning,
                        trip_EVs_returning_OD)
              }


            }
          }

          if (dim(trip_EVs_returning)[1] > 0) {
            # To assign trip start times
            # 9.3 Group-by EV ulid ------------
            # to find if an EV needs to make more than one trip per day
            trip_EVs_returning_g <-
              trip_EVs_returning %>%
              dplyr::group_by(.data$veh_id) %>%
              dplyr::summarise(n = dplyr::n())

            trip_EVs_returning$trip_start_time <- NA
            ii <- 1
            # These are the unique EVs in the set
            for (ii in 1:nrow(trip_EVs_returning_g)) {
              returning_trips_per_EV <- trip_EVs_returning_g$n[ii]
              jj <- 1
              for (jj in 1:returning_trips_per_EV) {
                # Make sure the next trip start time generated for the same EV
                # allows for the trip to the completed by an average speed
                # Assign this trip start time to the corresponding row of the trip_EVs df
                EV_id <-
                  trip_EVs_returning_g$veh_id[ii]
                # Find the corresponding OD pair, and trip distance
                trip_EV_returning_row <-
                  trip_EVs_returning[which(trip_EVs_returning$veh_id == EV_id)[jj],]

                if (dim(trip_EV_returning_row)[1] == 0) {

                }
                origin_zip <-
                  trip_EV_returning_row$origin_zip
                destination_zip <-
                  trip_EV_returning_row$destination_zip
                # Find the row corresponding to the OD pair
                trip_sp_ret <-
                  od_sp %>% dplyr::filter(.data$origin == origin_zip,
                                          .data$destination == destination_zip)
                # browser()
                # trip_cd_ret <-
                #   od_cd %>% dplyr::filter(.data$origin == origin_zip,
                #                           .data$destination == destination_zip)
                trip_dc_ret <-
                  dest_charger %>% dplyr::filter(.data$zip == destination_zip)
                # Find the distance for the OD pair
                dist <-
                  trip_sp_ret$shortest_path_length

                # Find the possible trip time rounded to hours and subtract from `det`
                # This means that a trip can start anywhere where trip start time and end
                # time such that it reaches the destination by around 10.00pm.
                trip_time <-
                  lubridate::hours(ceiling(dist / config$AVG_TRIP_SPEED))
                det_updated <- det - trip_time
                # If the trip is too long, then we might have to leave before 6.00am
                if (det_updated <= dst) {
                  dst <- det_updated
                }
                trip_start_time <-
                  rnd_date_time(
                    1,
                    st = dst,
                    et = det_updated,
                    tz = "America/Los_Angeles",
                    config = config
                  )
                # print(as.character(trip_start_time))

                trip_EV_returning_row$trip_start_time <-
                  as.character(trip_start_time)

                trip_EV_returning_row$dist <- dist

                # Find a start time that includes this trip time
                dst <-
                  trip_start_time + lubridate::hours(ceiling(dist / config$AVG_TRIP_SPEED))

                returning_trip_row <-
                  make_trip_row(
                    a_id = a_id,
                    main_con = main_con,
                    gas_prices =  wa_gas_prices,
                    trip_EV_row = trip_EV_returning_row,
                    trip_sp = trip_sp_ret,
                    trip_dc = trip_dc_ret
                  )

                prob_ij_bev <-
                  vcdm_scdm4(
                    ev_range = trip_EV_returning_row$range_fe,
                    trip_row = returning_trip_row,
                    config = config
                  )

                # print(prob_ij)

                set.seed(config[['GLOBAL_SEED']])
                # Make a random draw based on this probability
                ret_vehicle_choice <-
                  stats::rbinom(1, 1, prob_ij_bev)

                lg$log(
                  level = "info",
                  msg = "Calculating returning trip vehicle choice",
                  "ev_range" = trip_EV_returning_row$range_fe,
                  "trip_row" = returning_trip_row,
                  "prob_ij_bev" = prob_ij_bev,
                  "ret_vehicle_choice" = ret_vehicle_choice,
                  "origin_zip" = origin_zip,
                  "destination_zip" = destination_zip,
                  "veh_id" = EV_id,
                  "ip" = ipify::get_ip()
                )

                if (ret_vehicle_choice == 1) {
                  # Add the rows for an OD pair to the total dataframe
                  return_EVs <-
                    rbind(return_EVs,
                          trip_EV_returning_row)
                }
              }

              # Reset the trip start time limit
              dst <- paste(sim_day, config$START_TIME)
            }

          }
        }

        # Reset the trip start and end times
        dst <-
          lubridate::as_datetime(paste(sim_day, config$START_TIME), tz = "America/Los_Angeles")
        det <-
          lubridate::as_datetime(paste(sim_day, config$END_TIME), tz = "America/Los_Angeles")
        #print(returning_counter)
        returning_counter <- EV_req_tots$returning_trips[i]

        # departure ---------------------------------------------------------------

        # Iterate through each row - this iterates through an OD pair
        trip_EVs_departing <- data.frame()
        if (EV_req_tots$departing_trips[i] > 0) {
          departing_counter <- 1
          nz_departure_source <-
            nz_departure[nz_departure$origin == EV_req_tots$source[i],]
          for (j in 1:nrow(nz_departure_source)) {
            # Find the number of trips between the OD pair
            trip_count_OD <-
              nz_departure_source$long_distance_departure_trips[j]
            # print("returning_counter")
            # print(returning_counter)
            if ((trip_count_OD > 0) &&
                (
                  nrow(trip_EVs) >= returning_counter + departing_counter + trip_count_OD - 1
                )) {
              trip_EVs_departing_OD <-
                dplyr::slice(
                  trip_EVs,
                  (returning_counter + departing_counter):(
                    returning_counter + departing_counter + trip_count_OD - 1
                  )
                )
              # Increment the counter, so we select new EVs from trip_EVs
              departing_counter <-
                departing_counter + trip_count_OD

              if (dim(trip_EVs_departing_OD)[1] > 0) {
                trip_EVs_departing_OD$origin_zip <- nz_departure_source$origin[j]
                trip_EVs_departing_OD$destination_zip <-
                  nz_departure_source$destination[j]
                # Randomly assign SOCs to these vehicles, with replacement
                trip_EVs_departing_OD$soc <-
                  base::sample(
                    config$SOC_LOWER_LIMIT:config$SOC_UPPER_LIMIT,
                    trip_count_OD,
                    replace = TRUE
                  )

                trip_EVs_departing <-
                  rbind(trip_EVs_departing,
                        trip_EVs_departing_OD)
              }
            }


            # if (dim(trip_EVs_departing_OD)[1] == 0) {
            #     lg$fatal(msg = "trip_EVs_departing_OD has no rows just schema")
            #     browser()
            # }

          }

          if (dim(trip_EVs_departing)[1] > 0) {
            # To assign trip start times
            # Groupby EV ulid to find if an EV makes more than one trip per day
            trip_EVs_departing_g <-
              trip_EVs_departing %>% dplyr::group_by(.data$veh_id) %>% dplyr::summarise(n = dplyr::n())

            trip_EVs_departing$trip_start_time <- NA
            # These are the unique EVs in the set
            for (ii in 1:nrow(trip_EVs_departing_g)) {
              departing_trips_per_EV <- trip_EVs_departing_g$n[ii]

              for (jj in 1:departing_trips_per_EV) {
                # print(as.character(trip_start_time))
                # Assign this trip start time to the corresponding row of the trip_EVs df
                EV_id <-
                  trip_EVs_departing_g$veh_id[ii]

                # Make sure the next trip start time generated for the same EV
                # allows for the trip to the completed by an average speed

                # Find the corresponding OD pair, and trip distance
                trip_EV_departing_row <-
                  trip_EVs_departing[which(trip_EVs_departing$veh_id == EV_id)[jj],]

                if (dim(trip_EV_departing_row)[1] == 0) {
                  # browser()
                }
                origin_zip <-
                  trip_EV_departing_row$origin_zip
                destination_zip <-
                  trip_EV_departing_row$destination_zip
                # Find the row corresponding to the OD pair
                trip_sp_dep <-
                  od_sp %>% dplyr::filter(.data$origin == origin_zip,
                                          .data$destination == destination_zip)
                # trip_cd_dep <-
                #   od_cd %>% dplyr::filter(.data$origin == origin_zip,
                #                           .data$destination == destination_zip)
                trip_dc_dep <-
                  dest_charger %>% dplyr::filter(.data$zip == destination_zip)
                # Find the distance for the OD pair
                dist <-
                  trip_sp_dep$shortest_path_length
                # Find a start time that includes this trip time

                trip_time <-
                  lubridate::hours(ceiling(dist / config$AVG_TRIP_SPEED))

                det_updated <- det - trip_time
                # If the trip is too long, then we might have to leave before 6.00am
                if (det_updated <= dst) {
                  dst <- det_updated
                }
                trip_start_time <-
                  rnd_date_time(
                    1,
                    st = dst,
                    et = det_updated,
                    tz = "America/Los_Angeles",
                    config = config
                  )
                trip_EV_departing_row$trip_start_time <-
                  as.character(trip_start_time)

                trip_EV_departing_row$dist <- dist

                dst <-
                  trip_start_time + lubridate::hours(ceiling(dist / config$AVG_TRIP_SPEED))

                departing_trip_row <-
                  make_trip_row(
                    a_id = a_id,
                    main_con = main_con,
                    gas_prices =  wa_gas_prices,
                    trip_EV_row = trip_EV_departing_row,
                    trip_sp = trip_sp_dep,
                    trip_dc = trip_dc_dep
                  )

                prob_ij_bev <-
                  vcdm_scdm4(
                    ev_range = trip_EV_departing_row$range_fe,
                    trip_row = departing_trip_row,
                    config = config
                  )

                # print(prob_ij)

                set.seed(config[['GLOBAL_SEED']])

                dep_vehicle_choice <-
                  stats::rbinom(1, 1, prob_ij_bev)

                lg$log(
                  level = "info",
                  "ev_range" = trip_EV_departing_row$range_fe,
                  "trip_row" = departing_trip_row,
                  "prob_ij_bev" = prob_ij_bev,
                  "dep_vehicle_choice" = dep_vehicle_choice,
                  "origin_zip" = origin_zip,
                  "destination_zip" = destination_zip,
                  "veh_id" = EV_id,
                  "ip" = ipify::get_ip(),
                  msg = "Calculating departing trip vehicle choice"
                )

                if (dep_vehicle_choice == 1) {
                  # Add the rows for an OD pair to the total dataframe
                  # Binomial draw for one trial
                  departure_EVs <-
                    rbind(departure_EVs,
                          trip_EV_departing_row)
                }


              }

              # Reset the trip start time limit
              dst <- paste(sim_day, config$START_TIME)
            }
          }
        }

        # reset the counters
        returning_counter <- 1
        departing_counter <- 1

      }

      day_EVs <- data.frame()
      day_EVs <- rbind(return_EVs, departure_EVs)

      # print(day_EVs)
      # lg$info(day_EVs = day_EVs, msg = "day_EVs")
      if (dim(day_EVs)[1] > 0) {
        evtrips_zip <-
          day_EVs %>% dplyr::select(
            .data$veh_id,
            .data$origin_zip,
            .data$destination_zip,
            .data$soc,
            .data$trip_start_time
          ) %>% dplyr::mutate(analysis_id = a_id,
                              simulated_date = sim_day)

        DBI::dbAppendTable(main_con, 'evtrip_scenarios', evtrips_zip)
      }

    }
  }
  tst_df <-
    DBI::dbGetQuery(
      main_con,
      glue::glue(
        "select trip_start_time from
  evtrip_scenarios where analysis_id = {a_id}
  order by trip_start_time::timestamp
  limit 1;"
      )
    )

  query_status <-
    glue::glue(
      "update analysis_record set sim_start_time = '{tst_df$trip_start_time}', status = 'trips_generated' where analysis_id = {a_id}"
    )
  DBI::dbGetQuery(main_con, query_status)
  # DBI::dbRemoveTable(main_con, paste0("evses_now", a_id))
  DBI::dbDisconnect(main_con)
}

# trip_gen(num_days = 1)
