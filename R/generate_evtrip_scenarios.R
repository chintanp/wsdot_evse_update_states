a_id <- 14 #args[2]

# setwd(basedir)
source('./R/config.R')
# utility for checking is something is already installed, then loading.
usePackage <- function(p) {
    if (!is.element(p, utils::installed.packages()[, 1])) {
        # print(p)
        utils::install.packages(p, dep = TRUE, repos = "http://cran.us.r-project.org")
    }
    require(p, character.only = TRUE)
}

usePackage("dplyr")
usePackage("RPostgres")
usePackage("DBI")
usePackage("doParallel")
usePackage('lgr')
usePackage('jsonlite')
usePackage('rotor')

main_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("MAIN_HOST"),
    dbname = Sys.getenv("MAIN_DB"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD")
)

lg <- get_logger("test")$set_propagate(FALSE)$set_appenders(list(
    rotating = AppenderFileRotatingTime$new(
        file = "./R/logs/gen_evtrip.json",
        age = "1 day",
        layout = LayoutJson$new()
    )
))

lg$set_threshold("all")
lg$info(msg = "new run")
# Get the constants from the config file
# config <- config::get()

#' Generate random date times between two date-times, for a timezone
#' Adapted from : https://stackoverflow.com/a/14721124/1328232
#'
#' @param N Number of datetime values to generate
#' @param st Start date in the format "yyyy/mm/dd"
#' @param et End date in the format "yyyy/mm/dd"
#' @param tz Tiemzone for the dates
#'
#' @return The randomly generated date time.
#'
rnd_date_time <-
    function(N,
             st = "2019/07/01",
             et = "2019/07/02",
             tz = "America/Los_Angeles") {
        st <- as.POSIXct(st, tz = tz)
        et <- as.POSIXct(et, tz = tz)
        dt <- as.numeric(difftime(et, st, units = "secs"))
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
#'
#' @return The probability of travel by EV

vcdm_scdm4 <- function(ev_range, trip_row) {
    # Vehicle choice decision model parameters
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

    # Get the constants from the config file
    # config <- config::get()

    util_ij_ice <-
        theta_1 * trip_row$gas_price * trip_row$dist / AVG_FUEL_ECONOMY_OWN

    # Utility of driving a rental ICE
    util_ij_rent <-
        (theta_2 * AVG_RENTAL_CAR_COST) + (theta_3 * trip_row$gas_price * trip_row$dist / AVG_FUEL_ECONOMY_RENTAL)

    # utility of driving a BEV for the trip
    util_ij_bev <-
        ((theta_4 * (trip_row$dist / ev_range)) + (theta_5 * trip_row$max_spacing / ev_range) + (theta_6 * AVG_RESTROOM_SPACING) + theta_7 * 1 +
             (theta_8 * trip_row$dest_charger_L2) +
             (theta_9 * trip_row$dest_charger) + ASC_BEV
        )

    # Find the odds from utility
    # odds_ij <- exp(util_ij)
    # Find the probability from odds
    prob_ij_bev <-
        exp(util_ij_bev) / (exp(util_ij_bev) + exp(util_ij_rent) + exp(util_ij_ice))

    return(prob_ij_bev)
}

#' Create a dataframe that predicts the number of trips returning from origin to destination
#'
#' @param od A dataframe containing the results of the gravity model
#' @param od_sp Dataframe containing all trip rows
#' @param config config object loaded previously
#'
#' @return A dataframe with column origin, destination, and
#' number of trips between the origin and destination
#'
create_return_df <- function(od, od_sp) {

    #random draw from Poisson distribution
    daily_counts_ret <-
        stats::rpois(dim(od)[1], od$ret_calib_daily)

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
        return_distances %>% dplyr::filter(.data$shortest_path_length >= CRITICAL_DISTANCE)

    # Filter out OD pairs with non-zero trips
    nz_return <-
        return_distances_CD %>% dplyr::filter(.data$long_distance_return_trips > 0)

    return(nz_return)
}

#' Create a dataframe that predicts the number of trips departing from origin to destination
#'
#' @param od A dataframe containing the results of the gravity model
#' @param od_sp Dataframe containing OD and shortest_path_length
#' @param config config object loaded previously
#'
#' @return A dataframe with column origin, destination, and
#' number of trips between the origin and destination
#'
create_departure_df <- function(od, od_sp) {
    daily_counts_dep <-
        stats::rpois(dim(od)[1], od$dep_calib_daily)   #random draw from Poisson distribution

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
        departure_distances %>% dplyr::filter(.data$shortest_path_length >= CRITICAL_DISTANCE)

    nz_departure <-
        departure_distances_CD %>% dplyr::filter(.data$long_distance_departure_trips > 0)

    return(nz_departure)
}

#' Get the trips EVs from the relevant EVs belonging to a source zip code
#'
#'@param trips_source_i Number of trips from a source zip code
#'@param source_EVs the dataframe of EVs from the zip code
#'
#'@return The selected trip EVs from the set of EVs at source
#'
get_tripEVs_from_sourceEVs <- function(trips_source_i, source_EVs) {
    # An EV cannot make two trips in a day
    if (trips_source_i > nrow(source_EVs)) {
        trips_source_i <- nrow(source_EVs)
        # lg$fatal(msg = paste0(
        #     nrow(source_EVs),
        #     " source_EVs less than ",
        #     trips_source_i,
        #     " trips_source_i"
        # ))
    }
    # Randomly pick EVs from the relevant EVs "without replacement"
    trip_EVs <-
        dplyr::sample_n(source_EVs, trips_source_i, replace = FALSE)

    return(trip_EVs)
}

#' This function assign the origin and destination zip code to the trip EVs
#' and also assigns the lat, longs for the corresponding zip codes.
#'
#' @param trip_EVs_OD EVs making trips between origin and destination
#' @param origin_zip Zip code of origin
#' @param destination_zip Zip code of destination
#'
#' @return The dataframe with more details about origin and destination added
#'
# assign_OD_details <-
#     function(trip_EVs_OD, origin_zip, destination_zip) {
#         zipcode <-
#             DBI::dbGetQuery(main_con, "select * from zipcode_record where state = 'WA'")
#         # Assign origin and destination zip
#         # print(nz_return_source$origin[j])
#         trip_EVs_OD$origin_zip <- origin_zip
#
#         trip_EVs_OD$destination_zip <-
#             destination_zip
#         # Find origin and destination lat,lngs
#         olatLong <-
#             zipcode[zipcode$zip == origin_zip, 4:5]
#         trip_EVs_OD$Origin_Lat <-
#             olatLong$latitude
#         trip_EVs_OD$Origin_Lon <-
#             olatLong$longitude
#
#         dlatLong <-
#             zipcode[zipcode$zip == destination_zip, 4:5]
#         trip_EVs_OD$Destination_Lat <-
#             dlatLong$latitude
#         trip_EVs_OD$Destination_Lon <-
#             dlatLong$longitude
#
#         return(trip_EVs_OD)
#     }

#' This function returns the gas price for the zip code from the gas price dataframe
#'
#' @param gas_prices The dataframe containing gas price for each zip code
#' @param origin_zip The zip for which gas price is to be known
#'
#' @return The gas price for the relevant zip code
#'
get_Gas_Price <- function(gas_prices, origin_zip) {
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
#' @param gas_prices The dataframe containing gas prices for each zip code
#' @param trip_EV_row The dataframe containing details about the EV taking the trip
#' @param trip_sp Dataframe containing trip shortest_path
#' @param trip_cd Dataframe with trip charging_distances
#' @param trip_dc Dataframe with trip destination chargers
#'
#'
#' @return The dataframe with EV specific trip details
#'
make_trip_row <-
    function(gas_prices,
             trip_EV_row,
             trip_sp,
             trip_cd,
             trip_dc) {
        # lg$debug(
        #     trip_EV_row = trip_EV_row,
        #     trip_sp = trip_sp,
        #     trip_cd = trip_cd,
        #     trip_dc = trip_dc,
        #     msg = "in make_trip_row"
        # )
        # trip_cd is null, then give it path length
        if (rapportools::is.empty(trip_cd$cd_chademo)) {
            # lg$fatal(msg = paste("cd_chademo is null"),
            #          trip_EV_row = trip_EV_row)
            # In case the dataframe is empty create a new dataframe and add columns
            # trip_cd <- data.frame()
            # browser()
            trip_cd$cd_chademo <- numeric(nrow(trip_cd))
            trip_cd[1, 'cd_chademo'] <-
                trip_sp$shortest_path_length[1]
        }
        if (rapportools::is.empty(trip_cd$cd_combo)) {
            # lg$fatal(msg = paste("cd_combo is null"),
            #          trip_EV_row = trip_EV_row)
            #  browser()
            trip_cd$cd_combo <- numeric(nrow(trip_cd))
            trip_cd[1, 'cd_combo'] <-
                trip_sp$shortest_path_length[1]
        }

        if (trip_EV_row$connector_code == 1) {
            max_spacing <- trip_cd$cd_chademo # in miles
            dest_charger <-
                trip_dc$dc_chademo
        } else if (trip_EV_row$connector_code == 2) {
            max_spacing <- trip_cd$cd_combo
            dest_charger <-
                trip_dc$dc_combo
        } else {
            # Need to replace this with Tesla Superchargers
            max_spacing <-
                base::min(trip_cd$cd_chademo ,
                          trip_cd$cd_combo)
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

        # lg$debug(
        #     shortest_path_length = trip_sp$shortest_path_length,
        #     dest_charger_L2 = dest_charger_L2,
        #     dest_charger = dest_charger,
        #     max_spacing = max_spacing,
        #     gas_prices = gas_price,
        #     msg = "Debugging make_trip_row"
        # )
        trip_row <- data.frame(
            dist = trip_sp$shortest_path_length,
            dest_charger_L2 = dest_charger_L2,
            dest_charger = dest_charger,
            max_spacing = max_spacing,
            gas_price = gas_price
        )

        return(trip_row)
    }
#'
#' #'
#' #' Create the EV fleet for ABM simulation based on SDCM VCDM
#' #'
#' #' This function `trip_gen()` creates a fleet of EVs for EVI-ABM simulation
#' #' using the published models or vehicle choice using static choice discrete models.
#' #'
#' #'
#' #' @param num_days Number of days to generate the EV fleet for.
#' #'
#' #' @return Day EVs
#' #'
#' #' @export
#' #' @examples
#' #' f <- function(x) 42
#' #'
#' #' @importFrom rlang .data
#' #' @importFrom magrittr %>%
#' #' @importFrom utils data
#' #' @import zipcode
#' #'
#' trip_gen <- function(num_days = 1) {
#'     # Get the constants from the config file
#'     # config <- config::get()
#'
#'     # gas prices in WA
#'     wa_gas_prices <-
#'         DBI::dbGetQuery(main_con, 'select * from wa_gas_prices')
#'
#'     wa_bevs <- DBI::dbGetQuery(main_con, "select * from wa_bevs")
#'
#'     # These are the results of the EV trips generation from PJ
#'     wa_evtrips <-
#'         DBI::dbGetQuery(main_con, 'select * from wa_evtrips')
#'
#'     od_sp <-
#'         DBI::dbGetQuery(main_con, 'select * from od_sp')
#'
#'     od_cd <-
#'         DBI::dbGetQuery(
#'             main_con,
#'             paste0(
#'                 'select origin, destination, min(cd_chademo) as cd_chademo, min(cd_combo) as cd_combo from od_cd where analysis_id = -1 or analysis_id =  ',
#'                 a_id,
#'                 ' group by origin, destination;'
#'             )
#'         )
#'
#'     dest_charger <- DBI::dbGetQuery(
#'         main_con,
#'         paste0(
#'             'select zip, bool_or(dc_chademo) as dc_chademo, bool_or(dc_combo) as dc_combo, bool_or(dc_level2) as dc_level2 from dest_charger where analysis_id = -1 or analysis_id =  ',
#'             a_id,
#'             ' group by zip;'
#'         )
#'     )
#'     # all_trips_sp_df <-
#'     #     readRDS(here::here("R", "all_trips_sp_df.Rds"))
#'
#'     #daily EV trip counts
#'     wa_evtrips$dep_calib_daily <- wa_evtrips$dep * 354.3496 / 30
#'     wa_evtrips$ret_calib_daily <- wa_evtrips$ret * 354.3496 / 30
#'
#'     # Make all NA distances to zero, since we wont be able to traverse on them avayway
#'     # EVentually we filter all OD pairs where the distance is less than a threshold
#'     od_sp$shortest_path_length[which(is.na(od_sp$shortest_path_length))] <-
#'         0
#'
#'     returning_counter <- 0
#'     departing_counter <- 0
#'
#'     simi <- 1
#'     for (simi in 1:num_days) {
#'         #update this based on how many interation of data generation you need
#'
#'         #print(" Finding the returning and departing EV trips for the day ")
#'
#'         nz_return <-
#'             create_return_df(od = wa_evtrips, all_trips_sp_df = od_sp)
#'         nz_departure <-
#'             create_departure_df(od = wa_evtrips, all_trips_sp_df = od_sp)
#'         lg$debug(nz_return = nz_return,
#'                  msg = paste("nz_return with rows ", nrow(nz_return)))
#'         lg$debug(nz_departure =  nz_departure,
#'                  msg = paste("nz_departure with rows ", nrow(nz_departure)))
#'         # Find corresponding EV to make the trip
#'         return_EVs <- data.frame()
#'         departure_EVs <- data.frame()
#'
#'         # Get the simulation day from SIM_DATES
#'         sim_day <- SIM_DATES[simi]
#'
#'         # Groupby the final trips based the EV source - destination for returning trips and
#'         # origin for departing trips
#'         nz_return_g <-
#'             nz_return %>% dplyr::group_by(.data$destination) %>% dplyr::summarise(n = base::sum(.data$long_distance_return_trips))
#'         nz_departure_g <-
#'             nz_departure %>% dplyr::group_by(.data$origin) %>% dplyr::summarise(n = base::sum(.data$long_distance_departure_trips))
#'         lg$trace(nz_return_g = nz_return_g,
#'                  msg = paste("nz_return_g with rows ", nrow(nz_return_g)))
#'         lg$trace(nz_departure_g = nz_departure_g,
#'                  msg = paste("nz_departure with rows ", nrow(nz_departure_g)))
#'         #print("Finding total trips for a certain source")
#'
#'         # Merge the two dfs to get the total EV requirement per day
#'         EV_req <-
#'             base::merge(
#'                 nz_return_g,
#'                 nz_departure_g,
#'                 by.x = "destination",
#'                 by.y = "origin",
#'                 all = TRUE
#'             )
#'         # Fill NAs with 0
#'         data.table::setnafill(EV_req, fill =  0)
#'         # Find total trips from a source, and rename the column "destination" to "source"
#'         EV_req_tots <-
#'             EV_req %>% dplyr::mutate(total_trips = .data$n.x + .data$n.y) %>% dplyr::rename(
#'                 source = .data$destination,
#'                 returning_trips = .data$n.x,
#'                 departing_trips = .data$n.y
#'             )
#'         lg$trace(EV_req_tots = EV_req_tots,
#'                  msg = paste("EV_req_tots with rows ", nrow(EV_req_tots)))
#'         # Make random draws from the EVs at the source, for completing these many trips
#'         # These EVs will then be distributed into departing and returning trips
#'         i = 1
#'         for (i in 1:nrow(EV_req_tots)) {
#'             # Find corresponding EV to make the trip
#'             return_EVs <- data.frame()
#'             departure_EVs <- data.frame()
#'
#'             trips_source_i <- EV_req_tots$total_trips[i]
#'             # print(paste0(i, " trip_source: ", EV_req_tots$source[i]))
#'             lg$info(
#'                 msg = paste0(i, " trip_source: ", EV_req_tots$source[i]),
#'                 sources =
#'                     trips_source_i
#'             )
#'             # Find the EVs in the source zip code that could have made the trip
#'             source_EVs <-
#'                 wa_bevs %>% dplyr::filter(.data$zip_code == EV_req_tots$source[i])
#'             lg$debug(source_EVs = source_EVs,
#'                      msg = paste("source_EVs with rows ", nrow(source_EVs)))
#'             # If EVs are available in this zip code
#'             if (nrow(source_EVs) > 0) {
#'                 trip_EVs <-
#'                     get_tripEVs_from_sourceEVs(trips_source_i = trips_source_i,
#'                                                source_EVs = source_EVs)
#'                 # print("Generated a list of EVs for the days trips - now separate into return and departure")
#'
#'                 if (dim(trip_EVs)[1] == 0) {
#'                     lg$trace(msg = paste("trip_EVs has no rows just schema i = ", i))
#'                 }
#'                 # Randomly assign trip start times
#'                 dst <-
#'                     lubridate::as_datetime(paste(sim_day, START_TIME), tz = "America/Los_Angeles")
#'                 det <-
#'                     lubridate::as_datetime(paste(sim_day, END_TIME), tz = "America/Los_Angeles")
#'
#'                 # return ------------------------------------------------------------------
#'
#'                 # Iterate through each row - this iterates through an OD pair
#'                 trip_EVs_returning <- data.frame()
#'                 if (EV_req_tots$returning_trips[i] > 0) {
#'                     returning_counter <- 1
#'                     nz_return_source <-
#'                         nz_return[nz_return$destination == EV_req_tots$source[i], ]
#'                     j <- 1
#'                     for (j in 1:nrow(nz_return_source)) {
#'                         # Find the number of trips between the OD pair
#'                         trip_count_OD <-
#'                             nz_return_source$long_distance_return_trips[j]
#'
#'                         if (nrow(trip_EVs) <= returning_counter) {
#'                             trip_EVs_returning_OD <-
#'                                 dplyr::slice(
#'                                     trip_EVs,
#'                                     returning_counter:(
#'                                         returning_counter + trip_count_OD - 1
#'                                     )
#'                                 )
#'                             # Increment the counter, so we select new EVs from trip_EVs
#'                             returning_counter <-
#'                                 returning_counter + trip_count_OD
#'
#'                             lg$debug(trip_EVS_returning_OD = trip_EVs_returning_OD,
#'                                      msg = "trip_EVs_returning_OD")
#'
#'                             if (dim(trip_EVs_returning_OD)[1] > 0) {
#'                                 trip_EVs_returning_OD <-
#'                                     assign_OD_details(
#'                                         trip_EVs_returning_OD,
#'                                         origin_zip = nz_return_source$origin[j],
#'                                         destination_zip = nz_return_source$destination[j]
#'                                     )
#'
#'                                 # Randomly assign SOCs to these vehicles, with replacement
#'                                 trip_EVs_returning_OD$soc <-
#'                                     base::sample(
#'                                         SOC_LOWER_LIMIT:SOC_UPPER_LIMIT,
#'                                         trip_count_OD,
#'                                         replace = TRUE
#'                                     )
#'
#'                                 trip_EVs_returning <-
#'                                     rbind(trip_EVs_returning,
#'                                           trip_EVs_returning_OD)
#'                             }
#'
#'
#'                         }
#'                         # if (dim(trip_EVs_returning_OD)[1] == 0) {
#'                         #     lg$trace(
#'                         #         msg = paste(
#'                         #             "trip_EVs_returing_OD has no rows but schema, j: ",
#'                         #             j,
#'                         #             " i = ",
#'                         #             i
#'                         #         )
#'                         #     )
#'                         #     browser()
#'                         # }
#'
#'                     }
#'
#'                     if (dim(trip_EVs_returning)[1] > 0) {
#'                         # To assign trip start times
#'                         # Groupby EV ulid to find if an EV makes more than one trip per day
#'                         trip_EVs_returning_g <-
#'                             trip_EVs_returning %>% dplyr::group_by(.data$veh_id) %>% dplyr::summarise(n = dplyr::n())
#'                         trip_EVs_returning$trip_start_time <- NA
#'                         ii <- 1
#'                         # These are the unique EVs in the set
#'                         for (ii in 1:nrow(trip_EVs_returning_g)) {
#'                             returning_trips_per_EV <- trip_EVs_returning_g$n[ii]
#'                             jj <- 1
#'                             for (jj in 1:returning_trips_per_EV) {
#'                                 # Make sure the next trip start time generated for the same EV
#'                                 # allows for the trip to the completed by an average speed
#'                                 # Assign this trip start time to the corresponding row of the trip_EVs df
#'                                 EV_id <-
#'                                     trip_EVs_returning_g$veh_id[ii]
#'                                 # Find the corresponding OD pair, and trip distance
#'                                 trip_EV_returning_row <-
#'                                     trip_EVs_returning[which(trip_EVs_returning$veh_id == EV_id)[jj], ]
#'                                 print(paste("jj", jj, 'i', i, 'j', j))
#'
#'                                 # lg$info(paste("jj", jj, 'i', i, 'j', j))
#'                                 lg$debug(
#'                                     trip_EV_returning_row = trip_EV_returning_row,
#'                                     msg = paste("jj", jj, 'i', i, 'j', j)
#'                                 )
#'
#'                                 if (dim(trip_EV_returning_row)[1] == 0) {
#'                                     browser()
#'                                     lg$trace(
#'                                         msg = paste(
#'                                             "trip_EV_returning_row has no rows just schema, jj: ",
#'                                             jj,
#'                                             " j: ",
#'                                             j,
#'                                             " i = ",
#'                                             i,
#'                                             "ii: ",
#'                                             ii
#'                                         )
#'                                     )
#'                                 }
#'                                 origin_zip <-
#'                                     trip_EV_returning_row$origin_zip
#'                                 destination_zip <-
#'                                     trip_EV_returning_row$destination_zip
#'                                 # Find the row corresponding to the OD pair
#'                                 trip_sp_ret <-
#'                                     od_sp %>% dplyr::filter(
#'                                         .data$origin == origin_zip,
#'                                         .data$destination == destination_zip
#'                                     )
#'                                 trip_cd_ret <-
#'                                     od_cd %>% dplyr::filter(
#'                                         .data$origin == origin_zip,
#'                                         .data$destination == destination_zip
#'                                     )
#'                                 trip_dc_ret <-
#'                                     dest_charger %>% dplyr::filter(.data$zip == destination_zip)
#'                                 # Find the distance for the OD pair
#'                                 dist <-
#'                                     trip_sp_ret$shortest_path_length
#'
#'                                 # Find the possible trip time rounded to hours and subtract from `det`
#'                                 # This means that a trip can start anywhere where trip start time and end
#'                                 # time such that it reaches the destination by around 10.00pm.
#'                                 trip_time <-
#'                                     lubridate::hours(ceiling(dist / AVG_TRIP_SPEED))
#'                                 det_updated <- det - trip_time
#'                                 # If the trip is too long, then we might have to leave before 6.00am
#'                                 if (det_updated <= dst) {
#'                                     dst <- det_updated
#'                                 }
#'                                 trip_start_time <-
#'                                     rnd_date_time(
#'                                         1,
#'                                         st = dst,
#'                                         et = det_updated,
#'                                         tz = "America/Los_Angeles"
#'                                     )
#'                                 # print(as.character(trip_start_time))
#'
#'                                 trip_EV_returning_row$trip_start_time <-
#'                                     as.character(trip_start_time)
#'
#'                                 trip_EV_returning_row$dist <- dist
#'
#'                                 # Find a start time that includes this trip time
#'                                 dst <-
#'                                     trip_start_time + lubridate::hours(ceiling(dist / AVG_TRIP_SPEED))
#'
#'                                 returning_trip_row <-
#'                                     make_trip_row(
#'                                         gas_prices = wa_gas_prices,
#'                                         trip_EV_row = trip_EV_returning_row,
#'                                         trip_sp = trip_sp_ret,
#'                                         trip_cd = trip_cd_ret,
#'                                         trip_dc = trip_dc_ret
#'                                     )
#'
#'                                 prob_ij_bev <-
#'                                     vcdm_scdm4(
#'                                         ev_range = trip_EV_returning_row$electric_range,
#'                                         trip_row = returning_trip_row
#'                                     )
#'                                 # print(prob_ij)
#'                                 # Make a random draw based on this probability
#'                                 ret_vehicle_choice <-
#'                                     stats::rbinom(1, 1, prob_ij_bev)
#'
#'                                 if (ret_vehicle_choice == 1) {
#'                                     # Add the rows for an OD pair to the total dataframe
#'                                     return_EVs <-
#'                                         rbind(return_EVs,
#'                                               trip_EV_returning_row)
#'                                 }
#'                             }
#'
#'                             # Reset the trip start time limit
#'                             dst <- paste(sim_day, START_TIME)
#'                         }
#'
#'                     }
#'                 }
#'
#'                 # Reset the trip start and end times
#'                 dst <-
#'                     lubridate::as_datetime(paste(sim_day, START_TIME), tz = "America/Los_Angeles")
#'                 det <-
#'                     lubridate::as_datetime(paste(sim_day, END_TIME), tz = "America/Los_Angeles")
#'                 #print(returning_counter)
#'                 returning_counter <- EV_req_tots$returning_trips[i]
#'                 #print(returning_counter)
#'                 # print(return_EVs)
#'                 # departure ---------------------------------------------------------------
#'
#'                 # Iterate through each row - this iterates through an OD pair
#'                 trip_EVs_departing <- data.frame()
#'                 if (EV_req_tots$departing_trips[i] > 0) {
#'                     departing_counter <- 1
#'                     nz_departure_source <-
#'                         nz_departure[nz_departure$origin == EV_req_tots$source[i], ]
#'                     for (j in 1:nrow(nz_departure_source)) {
#'                         # Find the number of trips between the OD pair
#'                         trip_count_OD <-
#'                             nz_departure_source$long_distance_departure_trips[j]
#'                         # print("returning_counter")
#'                         # print(returning_counter)
#'                         if (nrow(trip_EVs) <= returning_counter + departing_counter) {
#'                             trip_EVs_departing_OD <-
#'                                 dplyr::slice(
#'                                     trip_EVs,
#'                                     (
#'                                         returning_counter + departing_counter
#'                                     ):(
#'                                         returning_counter + departing_counter + trip_count_OD - 1
#'                                     )
#'                                 )
#'                             # Increment the counter, so we select new EVs from trip_EVs
#'                             departing_counter <-
#'                                 departing_counter + trip_count_OD
#'
#'                             if (dim(trip_EVs_departing_OD)[1] > 0) {
#'                                 trip_EVs_departing_OD <-
#'                                     assign_OD_details(
#'                                         trip_EVs_OD = trip_EVs_departing_OD,
#'                                         origin_zip = nz_departure_source$origin[j],
#'                                         destination_zip = nz_departure_source$destination[j]
#'                                     )
#'                                 # Randomly assign SOCs to these vehicles, with replacement
#'                                 trip_EVs_departing_OD$soc <-
#'                                     base::sample(
#'                                         SOC_LOWER_LIMIT:SOC_UPPER_LIMIT,
#'                                         trip_count_OD,
#'                                         replace = TRUE
#'                                     )
#'
#'                                 trip_EVs_departing <-
#'                                     rbind(trip_EVs_departing,
#'                                           trip_EVs_departing_OD)
#'                             }
#'                         }
#'
#'
#'                         # if (dim(trip_EVs_departing_OD)[1] == 0) {
#'                         #     lg$fatal(msg = "trip_EVs_departing_OD has no rows just schema")
#'                         #     browser()
#'                         # }
#'
#'                     }
#'
#'                     if (dim(trip_EVs_departing)[1] > 0) {
#'                         # To assign trip start times
#'                         # Groupby EV ulid to find if an EV makes more than one trip per day
#'                         trip_EVs_departing_g <-
#'                             trip_EVs_departing %>% dplyr::group_by(.data$veh_id) %>% dplyr::summarise(n = dplyr::n())
#'
#'                         trip_EVs_departing$trip_start_time <- NA
#'                         # These are the unique EVs in the set
#'                         for (ii in 1:nrow(trip_EVs_departing_g)) {
#'                             departing_trips_per_EV <- trip_EVs_departing_g$n[ii]
#'
#'                             for (jj in 1:departing_trips_per_EV) {
#'                                 # print(as.character(trip_start_time))
#'                                 # Assign this trip start time to the corresponding row of the trip_EVs df
#'                                 EV_id <-
#'                                     trip_EVs_departing_g$veh_id[ii]
#'
#'                                 # Make sure the next trip start time generated for the same EV
#'                                 # allows for the trip to the completed by an average speed
#'
#'                                 # Find the corresponding OD pair, and trip distance
#'                                 trip_EV_departing_row <-
#'                                     trip_EVs_departing[which(trip_EVs_departing$veh_id == EV_id)[jj], ]
#'
#'                                 if (dim(trip_EV_departing_row)[1] == 0) {
#'                                     browser()
#'                                     lg$trace(
#'                                         msg = paste(
#'                                             "trip_EV_departing_row has no rows just schema, jj: ",
#'                                             jj,
#'                                             " j: ",
#'                                             j,
#'                                             " i = ",
#'                                             i,
#'                                             "ii: ",
#'                                             ii
#'                                         )
#'                                     )
#'                                 }
#'                                 origin_zip <-
#'                                     trip_EV_departing_row$origin_zip
#'                                 destination_zip <-
#'                                     trip_EV_departing_row$destination_zip
#'                                 # Find the row corresponding to the OD pair
#'                                 trip_sp_dep <-
#'                                     od_sp %>% dplyr::filter(
#'                                         .data$origin == origin_zip,
#'                                         .data$destination == destination_zip
#'                                     )
#'                                 trip_cd_dep <-
#'                                     od_cd %>% dplyr::filter(
#'                                         .data$origin == origin_zip,
#'                                         .data$destination == destination_zip
#'                                     )
#'                                 trip_dc_dep <-
#'                                     dest_charger %>% dplyr::filter(.data$zip == destination_zip)
#'                                 # Find the distance for the OD pair
#'                                 dist <-
#'                                     trip_sp_dep$shortest_path_length
#'                                 # Find a start time that includes this trip time
#'
#'                                 trip_time <-
#'                                     lubridate::hours(ceiling(dist / AVG_TRIP_SPEED))
#'
#'                                 det_updated <- det - trip_time
#'                                 # If the trip is too long, then we might have to leave before 6.00am
#'                                 if (det_updated <= dst) {
#'                                     dst <- det_updated
#'                                 }
#'                                 trip_start_time <-
#'                                     rnd_date_time(
#'                                         1,
#'                                         st = dst,
#'                                         et = det_updated,
#'                                         tz = "America/Los_Angeles"
#'                                     )
#'                                 trip_EV_departing_row$trip_start_time <-
#'                                     as.character(trip_start_time)
#'
#'                                 trip_EV_departing_row$dist <- dist
#'
#'                                 dst <-
#'                                     trip_start_time + lubridate::hours(ceiling(dist / AVG_TRIP_SPEED))
#'
#'                                 departing_trip_row <-
#'                                     make_trip_row(
#'                                         gas_prices = wa_gas_prices,
#'                                         trip_EV_row = trip_EV_departing_row,
#'                                         trip_sp = trip_sp_dep,
#'                                         trip_cd = trip_cd_dep,
#'                                         trip_dc = trip_dc_dep
#'                                     )
#'
#'                                 prob_ij_bev <-
#'                                     vcdm_scdm4(
#'                                         ev_range = trip_EV_departing_row$electric_range,
#'                                         trip_row = departing_trip_row
#'                                     )
#'
#'                                 # print(prob_ij)
#'                                 dep_vehicle_choice <-
#'                                     stats::rbinom(1, 1, prob_ij_bev)
#'
#'                                 if (dep_vehicle_choice == 1) {
#'                                     # Add the rows for an OD pair to the total dataframe
#'                                     # Binomial draw for one trial
#'                                     departure_EVs <-
#'                                         rbind(departure_EVs,
#'                                               trip_EV_departing_row)
#'                                 }
#'
#'
#'                             }
#'
#'                             # Reset the trip start time limit
#'                             dst <- paste(sim_day, START_TIME)
#'                         }
#'                     }
#'                 }
#'
#'                 # print("Generated trip EVs returning and departing with SOC and trip start times")
#'                 # Apply Yan's VCDM to each trip
#'                 # reset the counters
#'                 returning_counter <- 1
#'                 departing_counter <- 1
#'
#'             }
#'
#'             day_EVs <- data.frame()
#'             day_EVs <- rbind(return_EVs, departure_EVs)
#'
#'             print(day_EVs)
#'             lg$info(day_EVs = day_EVs, msg = "day_EVs")
#'             if (dim(day_EVs)[1] > 0) {
#'                 evtrips_zip <-
#'                     day_EVs %>% dplyr::select(veh_id,
#'                                               origin_zip,
#'                                               destination_zip,
#'                                               soc,
#'                                               trip_start_time) %>% dplyr::mutate(analysis_id = a_id,
#'                                                                                  simulated_date = sim_day)
#'
#'                 DBI::dbAppendTable(main_con, 'evtrip_scenarios', evtrips_zip)
#'             }
#'
#'             # utils::write.csv(
#'             #     day_EVs,
#'             #     file = here::here("trip_scenarios",
#'             #                       paste0(sim_day, ".csv")),
#'             #     row.names = FALSE
#'             # )
#'
#'             # write.csv(departure_EVs,
#'             #           file = here::here(
#'             #               "trip_scenarios",
#'             #               paste("evtrips_departure_", simi, ".csv", sep =
#'             #                         "")
#'             #           ))
#'             # return( day_EVs)
#'         }
#'
#'     }
#'
#'
#' }
#'
#' trip_gen(num_days = 1)
