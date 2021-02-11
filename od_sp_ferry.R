
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


od_sp <- DBI::dbGetQuery(main_con, 'select * from od_sp')

for (i in 1:nrow(od_sp)) {
  DBI::dbGetQuery(main_con, glue::glue('update od_sp set sp_len_ferry2 = (select sp_len_ferry({od_sp$origin[i]}, {od_sp$destination[i]})) where origin = {od_sp$origin[i]} and destination = {od_sp$destination[i]}'))
}
