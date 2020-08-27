#' Updates the trip infeasibility map
#'
#'
#'
#' @export
#'
#' @import magrittr
#' @importFrom utils data
#' @importFrom rlang .data
#'

update_trip_inf <- function () {
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

  all_trips <- DBI::dbGetQuery(main_con, "select * from all_trips")


  for (row in 1:nrow(all_trips)) {
    insert_query <-
      glue::glue(
        'insert into trip_infeasibility2 (trip_count, od_pairs, length, geom)
    (select at1.ccounts,
            at1.origin::text || at1.destination::text,
            spacings,
            st_linesubstring(line, ratio - lag, ratio)
     from all_trips at1
     CROSS JOIN LATERAL
         (select sq2.ratio,
                 (sq2.ratio - coalesce((lag(sq2.ratio) over (
                                                             order by sq2.ratio)), 0)) as lag,
                 ((sq2.ratio - coalesce((lag(sq2.ratio) over (
                                                              order by sq2.ratio)), 0)) * st_length(line::geography) * 0.000621371) as spacings,
                 line
          from
              (select ST_LineLocatePoint(line, sq.points) as ratio,
                      line
               from sp_od2(at1.origin::int, at1.destination::int) as line,

                   (select st_setsrid(st_makepoint(longitude, latitude), 4326) as points
                    from built_evse
                    where connector_code = 2
                        or connector_code = 3) as sq
               where st_dwithin(line::geography, sq.points::geography, 16093.4)
               union select 1.0,
                            line
               from sp_od2(at1.origin::int, at1.destination::int) as line
               order by ratio asc) as sq2) as sq3
     where at1."ObjectID" IN
             ({all_trips$ObjectID[row]})
         AND spacings > 70) on conflict (md5(geom::TEXT)) do
update
set trip_count = trip_infeasibility2.trip_count + EXCLUDED.trip_count,
    od_pairs = trip_infeasibility2.od_pairs || ', " ', '",
        ' || EXCLUDED.od_pairs;'
      )
    # print(insert_query)
    DBI::dbGetQuery(main_con, insert_query)
  }

}
