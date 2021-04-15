# This file contains tests for tripgen

# context("Testing tripgen")
# library(tripgen)

# Load the config
config <- config::get()

testthat::test_that("rnd_date_time() returns a date-time object", {
  # Test that N = 1 returns one date
  rt1  <-  tripgen::rnd_date_time(N = 1, config = config)
  # print(rt1)
  # print(lubridate::is.Date(rt1))
  testthat::expect_equal(lubridate::is.Date(as.Date(rt1)), TRUE)

  # Test that N != 1 also returns a vector of dates
  rt2 <-  tripgen::rnd_date_time(N = 2, config = config)
  testthat::expect_identical(vctrs::vec_ptype_abbr(as.Date(rt2)), "date")

  # Test if the resulting date is within the start and end dates

})

testthat::test_that("vcdm_scdm4() returns the vehicle choice probability", {
  ev_range1 = 100
  trip_row1 = data.frame(
    "gas_price" = 3.41,
    "dist" = 140,
    "max_spacing" = 110,
    "dest_charger" = TRUE,
    "dest_charger_L2" = TRUE
  )

  prob1 <-
    tripgen::vcdm_scdm4(ev_range = ev_range1,
                        config = config,
                        trip_row = trip_row1)
  #print(prob1)
  testthat::expect_equal(prob1, 0.005186169)

  ev_range2  <- 200
  trip_row2 <- data.frame(
    "gas_price" = 4.41,
    "dist" = 340,
    "max_spacing" = 210,
    "dest_charger" = FALSE,
    "dest_charger_L2" = FALSE
  )

  prob2 <-
    tripgen::vcdm_scdm4(ev_range = ev_range2,
                        config = config,
                        trip_row = trip_row2)
  # print(prob2)
  testthat::expect_equal(prob2, 0.04248596)

  ev_range3  <-  200
  trip_row3 <-  data.frame(
    "gas_price" = 4.41,
    "dist" = 340,
    "max_spacing" = 210,
    "dest_charger" = TRUE,
    "dest_charger_L2" = TRUE
  )

  prob3 <-
    tripgen::vcdm_scdm4(ev_range = ev_range3,
                        config = config,
                        trip_row = trip_row3)
  # print(prob3)
  testthat::expect_equal(prob3, 0.08053006)

})

testthat::test_that("create_return_df() return a DF with the right columns", {

})

testthat::test_that("create_departure_df() return a DF with the right columns", {

})
