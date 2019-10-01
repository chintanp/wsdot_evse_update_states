library("dplyr")
library("RPostgres")
library("DBI")
library("doParallel")
library('lgr')
library('jsonlite')
library('rotor')
library('magrittr')


# Logging settings --------------------------------------------------------

lg <- get_logger("test")$set_propagate(FALSE)$set_appenders(list(
  rotating = AppenderFileRotatingTime$new(
    file = "./R/logs/gen_evtrip.json",
    age = "1 day",
    layout = LayoutJson$new()
  )
))

lg$set_threshold("all")
lg$info(msg = "new run")

# Constants ---------------------------------------------------------------

# Minimum distance between OD pairs - FHWA specified critical limit
CRITICAL_DISTANCE <-  70
# SOC limits for random assignment
SOC_LOWER_LIMIT <-  80
SOC_UPPER_LIMIT <-  100
# Start and end times for trip beginning
# We generate random trip start times between these times
START_TIME <-  "06:00:00"
END_TIME <-  "22:00:00"
# Start and End date for simulation
# We generate trial EV agents to run simulations between these dates
SIM_START_DATE <- as.POSIXct("2019/07/01")
SIM_END_DATE <- as.POSIXct("2019/08/01")
SIM_DATES <- seq.Date(
  from = as.Date(SIM_START_DATE),
  to = as.Date(SIM_END_DATE),
  by = "day"
)

# A low trip average speed for calculating subsequent trips
AVG_TRIP_SPEED <- 40 # mph

# Cost of rental per day - assumed constant across WA - can change with zip
AVG_RENTAL_CAR_COST <- 50 # $ per day (or $ per trip here)

# Average fuel economy of rental car
AVG_FUEL_ECONOMY_RENTAL <-
  25 # mpg, since rental cars are generally newer

# Average fuel economy of a user owned car
AVG_FUEL_ECONOMY_OWN <- 23 # mpg

# Furthest restroom break
AVG_RESTROOM_SPACING <- 20 # miles
