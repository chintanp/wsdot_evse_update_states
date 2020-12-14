#
# if (!DBI::dbCanConnect(
#   RPostgres::Postgres(),
#   host = Sys.getenv("MAIN_HOST"),
#   dbname = Sys.getenv("MAIN_DB"),
#   user = Sys.getenv("MAIN_USER"),
#   password = Sys.getenv("MAIN_PWD"),
#   port = Sys.getenv("MAIN_PORT")
# )) {
#   lg$log(level = "fatal",
#          msg = "Cannot connect to database",
#          "ip" = ipify::get_ip())
#   # Exit if DB cannot connect
#   stop("Cannot connect to database")
# }
#
# main_con <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   host = Sys.getenv("MAIN_HOST"),
#   dbname = Sys.getenv("MAIN_DB"),
#   user = Sys.getenv("MAIN_USER"),
#   password = Sys.getenv("MAIN_PWD"),
#   port = Sys.getenv("MAIN_PORT")
# )
#
#
# aids <- DBI::dbGetQuery(main_con, 'select analysis_id from analysis_record')$analysis_id
#
# print(aids)
#
# for (a_id in aids) {
#   tripgen::make_evses_now_table(main_con, a_id)
# }
#
