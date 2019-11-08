# library(update.states)
library(xml2)

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
  trip_gen(num_days = 1,
           config = config,
           a_id = a_id)
  print("trips generated")
  # Create an xml file with analysis_id as a parameter
  xml1 <- read_xml("R/GAMA/headless/WSDOT_EV/sim_day_db_waiting.xml")
  ps <- xml2::xml_children(xml1)
  psc <- xml2::xml_children(ps[[1]])
  xml2::xml_attr(psc[[1]], "value") <- as.character(a_id)
  xml2::xml_attr(psc[[2]], "value") <- Sys.getenv("MAIN_HOST")
  xml2::xml_attr(psc[[3]], "value") <- 'Postgres'
  xml2::xml_attr(psc[[4]], "value") <- Sys.getenv("MAIN_DB")
  xml2::xml_attr(psc[[5]], "value") <- Sys.getenv("MAIN_PORT")
  xml2::xml_attr(psc[[6]], "value") <- Sys.getenv("MAIN_USER")
  xml2::xml_attr(psc[[7]], "value") <- Sys.getenv("MAIN_PWD")
  xml2::write_xml(xml1,
                  paste0("R/GAMA/headless/WSDOT_EV/sim_day_db_waiting", a_id, ".xml"))
  # Run the GAMA simulation with the updated parameter
  cmd <- paste0(
    here::here("R/GAMA/headless/gama-headless.bat")," -m 10G ",
    paste0(here::here(), "/R/GAMA/headless/WSDOT_EV/sim_day_db_waiting", a_id, ".xml "),
    paste0(
      here::here(), "/R/GAMA/headless/WSDOT_EV/headless_output/sim_day_db_waiting",
      a_id
    )
  )

  write.table(cmd, file = paste0("R/GAMA/headless/", a_id, ".bat"), quote = FALSE, col.names = FALSE, row.names = FALSE)
  setwd(here::here("R/GAMA/headless/"))
  print(getwd())
  print(paste0(here::here(), "/R/GAMA/headless/", a_id, ".bat"))
  system2(
    paste0(here::here(), "/R/GAMA/headless/", a_id, ".bat"),
    stdout = paste0(here::here(), "/R/GAMA/headless/WSDOT_EV/rstdout/", a_id, ".txt"),
    stderr = paste0(here::here(), "/R/GAMA/headless/WSDOT_EV/rstderr/", a_id, ".txt")
  )
}

args <- commandArgs(TRUE)
print(args)
a_id <- args[2]

update_states_and_gen_trips(a_id)
