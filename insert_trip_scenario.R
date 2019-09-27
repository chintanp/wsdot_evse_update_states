# This file inserts the test trip scenario
# utility for checking is something is already installed, then loading.
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[, 1])) {
        install.packages(p, dep = TRUE, repos = "http://cran.us.r-project.org")
    }
    require(p, character.only = TRUE)
}


usePackage("dplyr")
usePackage("RPostgres")
usePackage("DBI")
usePackage("doParallel")
usePackage("vroom")

main_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = "localhost",
    dbname = "wsdot_evse_main",
    user = Sys.getenv("MAIN_USERNAME"),
    password = Sys.getenv("MAIN_PWD")
)

trip_scenario_df <-
    vroom::vroom("data-raw/2019-07-01.csv", delim = ",")

trip_scenario_df <-
    trip_scenario_df %>% dplyr::select(ulid, origin_zip, destination_zip, SOC, trip_start_time) %>% dplyr::rename(veh_id = ulid, soc = SOC)
trip_scenario_df <-
    trip_scenario_df %>% tibble::add_column(simulated_date = "2019-07-01", analysis_id = 21)

DBI::dbAppendTable(main_con, "evtrip_scenarios", trip_scenario_df)
DBI::dbDisconnect(main_con)
