# library(update.states)
library(xml2)
library(lgr)
library(ipify)

source('R/update_cd.R')
source("R/update_dc.R")
source("R/generate_evtrip_scenarios.R")
source("R/setup_logging.R")

update_states_and_gen_trips <- function(a_id = 20) {
  config <- config::get()

  lg <- get_logger("test")$set_propagate(FALSE)$set_appenders(lgr::AppenderJson$new(layout = LayoutLogstash$new(), file = here::here(paste0("logs/runner_", as.character(a_id), ".log"))))

  lg$log(level = "info", msg = "Config loaded", "config" = config, "ip" = get_ip())

  # print(str(config))
  update_dc(a_id)

  print("destination charger updated")
  lg$log(level = "info", msg = "Destination chargers updated", "ip" = get_ip())

  update_cd(config = config, a_id = a_id)

  print("charging distances updated")
  lg$log(level = "info", msg = "charging distances updated", "ip" = get_ip())

  trip_gen(num_days = 1,
           config = config,
           a_id = a_id)

  print("trips generated")
  lg$log(level = "info", msg = "trips generated", "ip" = get_ip())

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
    here::here("R/GAMA/jdk/bin/java")," -cp ", here::here("R/GAMA/plugins/org.eclipse.equinox.launcher_1.5.300.v20190213-1655.jar"), " -Xmx20000m -Djava.awt.headless=true org.eclipse.core.launcher.Main  -application msi.gama.headless.id4 ",
    paste0(here::here(), "/R/GAMA/headless/WSDOT_EV/sim_day_db_waiting", a_id, ".xml "),
    paste0(
      here::here(), "/R/GAMA/headless/WSDOT_EV/headless_output/sim_day_db_waiting",
      a_id
    )
  )

  write.table(cmd, file = paste0("R/GAMA/headless/", a_id, ".bat"), quote = FALSE, col.names = FALSE, row.names = FALSE)
  setwd(here::here("R/GAMA/headless/"))

  lg$log(level = "info", msg = "Starting ABM simulation in GAMA for analysis_id: %s", as.character(a_id), "ip" = get_ip())

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
