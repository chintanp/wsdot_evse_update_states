# This file tests the function `rnd_date_time()`

testthat::test_that("rnd_date_time returns a POXISct datetime object", {
  testthat::expect_is(rnd_date_time(N = 1), 'POSIXct')
  testthat::expect_is(rnd_date_time(N = 1, st = "2019-07-01 15:45:56"), "POSIXct")
  testthat::expect_is(
    rnd_date_time(N = 1, st = "2019-07-01 15:45:56", et = "2019-07-01 18:23:34"),
    "POSIXct"
  )
})
