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

  test_config <- data.frame(AVG_FUEL_ECONOMY_OWN = 25,
                            AVG_FUEL_ECONOMY_RENTAL = 26,
                            AVG_RESTROOM_SPACING = 20,
                            AVG_RENTAL_CAR_COST = 50)

  testthat::expect_lte(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row1, config = test_config), 1.0)
  testthat::expect_gte(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row1, config = test_config), 0.0)
  testthat::expect_lte(vcdm_scdm4(ev_range = test_ev_range2, trip_row = test_trip_row2, config = test_config), 1.0)
  testthat::expect_gte(vcdm_scdm4(ev_range = test_ev_range2, trip_row = test_trip_row2, config = test_config), 0.0)
  testthat::expect_is(vcdm_scdm4(ev_range = test_ev_range2, trip_row = test_trip_row2, config = test_config), 'numeric')

})


testthat::test_that("vcdm_scdm4 returns error on wrong inputs", {
  test_config <- data.frame(AVG_FUEL_ECONOMY_OWN = 25,
                            AVG_FUEL_ECONOMY_RENTAL = 26,
                            AVG_RESTROOM_SPACING = 20,
                            AVG_RENTAL_CAR_COST = 50)
  test_ev_range1 <- 124.6

  test_trip_row3 <-
    data.frame(
      gas_price = c(1.3, 4.5),
      dist = c(100.5, 78.9),
      dest_charger_L2 = c(1, 0),
      dest_charger = c(0, 1),
      max_spacing = c(123.4, 45.6)
    )

  test_trip_row4 <-
    data.frame(
      gas_price = numeric(),
      dist = numeric(),
      dest_charger_L2 = logical(),
      dest_charger = logical(),
      max_spacing = numeric()
    )
  test_trip_row1 <-
    data.frame(
      gas_price = numeric(),
      dist = numeric(),
      dest_charger_L2 = logical(),
      dest_charger = logical(),
      max_spacing = numeric()
    )
  test_trip_row1[1,] <- c(3.5, 150.4, 1, 0, 120.2)

  test_config2 <- data.frame(
                            AVG_FUEL_ECONOMY_RENTAL = 26,
                            AVG_RESTROOM_SPACING = 20,
                            AVG_RENTAL_CAR_COST = 50)
  test_config3 <- data.frame(
    AVG_FUEL_ECONOMY_OWN = 26,
    AVG_RESTROOM_SPACING = 20,
    AVG_RENTAL_CAR_COST = 50)
  test_config4 <- data.frame(
    AVG_FUEL_ECONOMY_OWN = 26,
    AVG_FUEL_ECONOMY_RENTAL = 26,
    AVG_RENTAL_CAR_COST = 50)
  test_config5 <- data.frame(
    AVG_FUEL_ECONOMY_OWN = 26,
    AVG_FUEL_ECONOMY_RENTAL = 26,
    AVG_RESTROOM_SPACING = 20)

  test_ev_range2 <- 'abc'
  test_ev_range3 <- -124.6

  testthat::expect_error(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row4, config = test_config), 'trip_row has 0 rows - should have just 1')
  testthat::expect_error(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row3, config = test_config), 'trip_row has more than 1 rows - should have just 1')
  testthat::expect_error(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row1, config = test_config2), 'config is missing the field `AVG_FUEL_ECONOMY_OWN`')
  testthat::expect_error(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row1, config = test_config3), 'config is missing the field `AVG_FUEL_ECONOMY_RENTAL`')
  testthat::expect_error(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row1, config = test_config4), 'config is missing the field `AVG_RESTROOM_SPACING`')
  testthat::expect_error(vcdm_scdm4(ev_range = test_ev_range1, trip_row = test_trip_row1, config = test_config5), 'config is missing the field `AVG_RENTAL_CAR_COST`')
  testthat::expect_error(vcdm_scdm4(ev_range = test_ev_range2, trip_row = test_trip_row1, config = test_config), 'ev_range is of non-numeric type - provide numeric value for ev_range')
  testthat::expect_error(vcdm_scdm4(ev_range = test_ev_range3, trip_row = test_trip_row1, config = test_config), 'ev_range should be a positive value')
})
