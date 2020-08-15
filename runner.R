update_states_and_gen_trips <- function(a_id = 1) {
  config <- config::get()

  tripgen::update_dc(a_id)

  print("destination charger updated")

  tripgen::trip_gen(num_days = 1,
           config = config,
           a_id = a_id)
}

a_id <-  read.table("analysis_id", header = F)[1, 1]
update_states_and_gen_trips(a_id)

