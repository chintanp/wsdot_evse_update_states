# This scripts tests the function `vcdm_scdm4()`

testthat::test_that("vcdm_scdm4 returns number between 0 and 1", {
  test_trip_row1 <-
    data.frame(
      gas_price = numeric(),
      dist = numeric(),
      dest_charger_L2 = logical(),
      dest_charger = logical(),
      max_spacing = numeric()
    )
  test_trip_row2 <-
    data.frame(
      gas_price = numeric(),
      dist = numeric(),
      dest_charger_L2 = logical(),
      dest_charger = logical(),
      max_spacing = numeric()
    )
  test_trip_row1[1,] <- c(3.5, 150.4, 1, 0, 120.2)
  test_ev_range1 <- 124.6
  test_trip_row2[1,] <- c(10, 30, 0, 1, 300)
  test_ev_range2 <- 4.6
  testthat::expect_lte(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row1), 1.0)
  testthat::expect_gte(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row1), 0.0)
  testthat::expect_lte(vcdm_scdm4(ev_range = test_ev_range2, trip_row = test_trip_row2), 1.0)
  testthat::expect_gte(vcdm_scdm4(ev_range = test_ev_range2, trip_row = test_trip_row2), 0.0)
  testthat::expect_is(vcdm_scdm4(ev_range = test_ev_range2, trip_row = test_trip_row2), 'numeric')
})
