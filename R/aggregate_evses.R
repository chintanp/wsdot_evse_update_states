# This script aggregates the close-by evses into 1 evse, so have more plugs available

library(RPostgres)
library(DBI)
library(dplyr)

a_id <- 104

main_con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("MAIN_HOST"),
  dbname = Sys.getenv("MAIN_DB"),
  user = Sys.getenv("MAIN_USER"),
  password = Sys.getenv("MAIN_PWD"),
  port = Sys.getenv("MAIN_PORT")
)


bevse_query <- "SELECT dcfc_count as dcfc_plug_count, longitude, latitude, 'b' || bevse_id::text as evse_id , connector_code, dcfc_fixed_charging_price, dcfc_var_charging_price_unit, dcfc_var_charging_price, dcfc_fixed_parking_price, dcfc_var_parking_price_unit, dcfc_var_parking_price FROM built_evse where dcfc_count >= 1;"
nevse_query <- paste0("SELECT 'n' || nevse_id::text as evse_id, longitude, latitude, connector_code, dcfc_plug_count, dcfc_fixed_charging_price, dcfc_var_charging_price_unit, dcfc_var_charging_price, dcfc_fixed_parking_price, dcfc_var_parking_price_unit, dcfc_var_parking_price from new_evses where dcfc_plug_count > 0 and analysis_id = ",  a_id)


bevses <- DBI::dbGetQuery(main_con, bevse_query)
nevses <- DBI::dbGetQuery(main_con, nevse_query)

evses <- rbind(bevses, nevses)

DBI::dbWriteTable(main_con, paste0("evses_now", a_id), evses, overwrite = FALSE)

dist_mat_query <- sprintf("
SELECT
  t1.evse_id AS t1,
  t2.evse_id AS t2,
  ST_Distance(st_setsrid(st_makepoint(t1.longitude, t1.latitude), 4326)::geography, st_setsrid(st_makepoint(t2.longitude, t2.latitude), 4326)::geography)
FROM %s AS t1
INNER JOIN %s AS t2 on (st_setsrid(st_makepoint(t1.longitude, t1.latitude), 4326) < st_setsrid(st_makepoint(t2.longitude, t2.latitude), 4326));", paste0("evses_now", a_id), paste0("evses_now", a_id))

dist_mat <- DBI::dbGetQuery(main_con, dist_mat_query)
dist_mat_200 <- dist_mat %>% dplyr::filter(st_distance <= 200) %>% dplyr::arrange(st_distance)

for (i in 1:nrow(dist_mat_200)) {
  evse_1_id <- dist_mat_200$t1[i]
  evse_2_id <- dist_mat_200$t2[i]

  if (evses$connector_code[evses$evse_id == evse_1_id] != 4 & (evses$connector_code[evses$evse_id == evse_1_id] == evses$connector_code[evses$evse_id == evse_2_id])) {
    print(evses$connector_code[evses$evse_id == evse_1_id])
    print(evses$connector_code[evses$evse_id == evse_2_id])
    print("combining")

    evses$dcfc_plug_count[evses$evse_id == evse_1_id] <- evses$dcfc_plug_count[evses$evse_id == evse_1_id] + evses$dcfc_plug_count[evses$evse_id == evse_2_id]

    evses$evse_id[evses$evse_id == evse_1_id] <- paste(evse_1_id, evse_2_id, sep = ",")

  }
}
