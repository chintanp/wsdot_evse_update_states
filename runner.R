update_states_and_gen_trips <- function(a_id = 1) {

  source("./R/setup_logging.R")
  ## Setup the logging destination
  lg <-
    lgr::get_logger("test")$set_propagate(FALSE)$set_appenders(lgr::AppenderJson$new(layout = LayoutLogstash$new(), file = here::here(
      paste0("logs/runner_", as.character(a_id), ".log")
    )))

  config <- config::get()

  # Get parameters from the DB
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

  # Get params for the analysis id
  analysis_params <-
    DBI::dbGetQuery(main_con, glue::glue("select sp.param_name, ap.param_value from analysis_params ap
JOIN sim_params sp on ap.param_id = sp.param_id
where ap.analysis_id = {a_id};"))

  config[['GLOBAL_SEED']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'global_seed'])
  config[['CRITICAL_DISTANCE']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'critical_distance_miles'])
  config[['SOC_LOWER_LIMIT']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'soc_lower_limit_pc'])
  config[['SOC_UPPER_LIMIT']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'soc_upper_limit_pc'])
  config[['AVG_TRIP_SPEED']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'avg_trip_speed_mph'])
  config[['AVG_RENTAL_CAR_COST']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'avg_rental_car_cost_usd'])
  config[['AVG_FUEL_ECONOMY_RENTAL']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'avg_fuel_economy_rental_mpg'])
  config[['AVG_FUEL_ECONOMY_OWN']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'avg_fuel_economy_own_mpg'])
  config[['AVG_RESTROOM_SPACING']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'avg_restroom_spacing_miles'])
  config[['LOOKUP_DISTANCE']] <- as.numeric(analysis_params$param_value[analysis_params$param_name == 'lookup_distance_miles'])

  # browser()
  set.seed(config[['GLOBAL_SEED']])

  tripgen::update_dc(a_id)

  print("destination charger updated")

  tripgen::trip_gen(num_days = 1,
           config = config,
           a_id = a_id)
}

a_id <-  read.table("analysis_id", header = F)[1, 1]
update_states_and_gen_trips(a_id)

