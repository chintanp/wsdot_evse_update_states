% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_evtrip_scenarios.R
\name{make_evses_now_table}
\alias{make_evses_now_table}
\title{Make EVSES_NOW table}
\usage{
make_evses_now_table(main_con, a_id = 1)
}
\arguments{
\item{main_con}{The database connection to use to interact with the database}

\item{a_id}{analysis_id}
}
\value{
evses_now dataframe
}
\description{
This function `make_evses_now_table()` creates a database table which contains
the evses for this analysis. This is a combination of existing EVSES and proposed EVSES
}
