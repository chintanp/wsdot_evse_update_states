library(update.states)

source('R/update_cd.R')
source("R/update_dc.R")
source("R/generate_evtrip_scenarios.R")

update_states_and_gen_trips <- function(a_id) {
  config <- config::get()
  print(str(config))
  update_dc(a_id)
  print("destination charger updated")
  update_cd(config = config, a_id = a_id)
  print("charging distances updated")
  trip_gen(num_days = 1, config = config, a_id = a_id)
  print("trips generated")
}

args <- commandArgs(TRUE)
print(args)
a_id <- args[2]

update_states_and_gen_trips(a_id)
