testthat::test_that("create_departure_df returns a dataframe with necessary columns", {
  test_od1 <-
    data.frame(
      "origin" = integer(),
      "destination" = integer(),
      "ret" = numeric(),
      "dep" = numeric(),
      "oevs" = integer(),
      "devs" = integer(),
      "ocars" = integer(),
      "dcars" = integer(),
      "dep_calib_daily" = numeric(),
      "ret_calib_daily" = numeric()
    )
  test_od2 <- test_od1
  test_od_sp1 <-
    data.frame(
      origin = 98001,
      destination = 98002,
      shortest_path_length = 100.56
    )

  test_od1[1,] <- c(98001, 98002, 45.65, 34.78, 44, 55, 345, 567, 45.79, 23.45)
  test_od2[1,] <- c(98001, 98002, 248.689354024024, 237.99198546772, 66, 34, 11097, 13111, 2810.979, 2937.242)

  test_config <- data.frame(CRITICAL_DISTANCE = 70)

  # test_od_sp1 <- c(98001, 98002, 34.56)
  testthat::expect_is(create_departure_df(od = test_od1, od_sp = test_od_sp1, config = test_config), 'data.frame')
  testthat::expect_length(colnames(create_departure_df(od = test_od1, od_sp = test_od_sp1, config = test_config)), 4)
  testthat::expect_setequal(colnames(create_departure_df(od = test_od1, od_sp = test_od_sp1, config = test_config)), c('origin', 'destination', 'long_distance_departure_trips', 'shortest_path_length'))
  testthat::expect_is(create_departure_df(od = test_od2, od_sp = test_od_sp1, config = test_config), 'data.frame')
  testthat::expect_length(colnames(create_departure_df(od = test_od2, od_sp = test_od_sp1, config = test_config)), 4)
  testthat::expect_setequal(colnames(create_departure_df(od = test_od2, od_sp = test_od_sp1, config = test_config)), c('origin', 'destination', 'long_distance_departure_trips', 'shortest_path_length'))
})


testthat::test_that("create_departure_df throws an error on wrong input", {
  test_od1 <-
    data.frame(
      "origin" = integer(),
      "destination" = integer(),
      "ret" = numeric(),
      "dep" = numeric(),
      "oevs" = integer(),
      "devs" = integer(),
      "ocars" = integer(),
      "dcars" = integer(),
      "dep_calib_daily" = numeric(),
      "ret_calib_daily" = numeric()
    )
  test_od_sp1 <-
    data.frame(
      origin = 98001,
      destination = 98002,
      shortest_path_length = 100.56
    )
  test_config <- data.frame(CRITICAL_DISTANCE = 70)

  test_od2 <-
    data.frame(
      "destination" = integer(),
      "ret" = numeric(),
      "dep" = numeric(),
      "oevs" = integer(),
      "devs" = integer(),
      "ocars" = integer(),
      "dcars" = integer(),
      "dep_calib_daily" = numeric(),
      "ret_calib_daily" = numeric()
    )
  test_od3 <-
    data.frame(
      "origin" = integer(),
      "ret" = numeric(),
      "dep" = numeric(),
      "oevs" = integer(),
      "devs" = integer(),
      "ocars" = integer(),
      "dcars" = integer(),
      "dep_calib_daily" = numeric(),
      "ret_calib_daily" = numeric()
    )
  test_od4 <-
    data.frame(
      "origin" = integer(),
      "destination" = integer(),
      "ret" = numeric(),
      "dep" = numeric(),
      "oevs" = integer(),
      "devs" = integer(),
      "ocars" = integer(),
      "dcars" = integer(),
      "ret_calib_daily" = numeric()
    )
  test_od5 <-
    data.frame(
      "origin" = integer(),
      "destination" = integer(),
      "ret" = numeric(),
      "dep" = numeric(),
      "devs" = integer(),
      "ocars" = integer(),
      "dcars" = integer(),
      "dep_calib_daily" = numeric(),
      "ret_calib_daily" = numeric()
    )
  test_od6 <-
    data.frame(
      "origin" = integer(),
      "destination" = integer(),
      "ret" = numeric(),
      "dep" = numeric(),
      "oevs" = integer(),
      "devs" = integer(),
      "dcars" = integer(),
      "dep_calib_daily" = numeric(),
      "ret_calib_daily" = numeric()
    )
  test_od2[1,] <- c(98002, 248.689354024024, 237.99198546772, 66, 34, 11097, 13111, 2810.979, 2937.242)
  test_od3[1,] <- c(98001, 248.689354024024, 237.99198546772, 66, 34, 11097, 13111, 2810.979, 2937.242)
  test_od4[1,] <- c(98001, 98002, 248.689354024024, 237.99198546772, 66, 34, 11097, 13111, 2937.242)
  test_od5[1,] <- c(98001, 98002, 248.689354024024, 237.99198546772, 34, 11097, 13111, 2810.979, 2937.242)
  test_od6[1,] <- c(98001, 98002, 248.689354024024, 237.99198546772, 66, 34, 13111, 2810.979, 2937.242)

  testthat::expect_error(create_departure_df(od = test_od1, od_sp = test_od_sp1, config = test_config), 'od has 0 rows - should have atleast 1')
  testthat::expect_error(create_departure_df(od = test_od2, od_sp = test_od_sp1, config = test_config), 'od is missing the necessary column origin')
  testthat::expect_error(create_departure_df(od = test_od3, od_sp = test_od_sp1, config = test_config), 'od is missing the necessary column destination')
  testthat::expect_error(create_departure_df(od = test_od4, od_sp = test_od_sp1, config = test_config), 'od is missing the necessary column dep_calib_daily')
  testthat::expect_error(create_departure_df(od = test_od5, od_sp = test_od_sp1, config = test_config), 'od is missing the necessary column oevs')
  testthat::expect_error(create_departure_df(od = test_od6, od_sp = test_od_sp1, config = test_config), 'od is missing the necessary column ocars')

  test_od <-
    data.frame(
      "origin" = integer(),
      "destination" = integer(),
      "ret" = numeric(),
      "dep" = numeric(),
      "oevs" = integer(),
      "devs" = integer(),
      "ocars" = integer(),
      "dcars" = integer(),
      "dep_calib_daily" = numeric(),
      "ret_calib_daily" = numeric()
    )
  test_od[1,] <- c(98001, 98002, 248.689354024024, 237.99198546772, 66, 34, 11097, 13111, 2810.979, 2937.242)
  test_od_sp2 <-
    data.frame(
      destination = 98002,
      shortest_path_length = 100.56
    )
  test_od_sp3 <-
    data.frame(
      origin = 98001,
      shortest_path_length = 100.56
    )
  test_od_sp4 <-
    data.frame(
      origin = 98001,
      destination = 98002
    )

  testthat::expect_error(create_departure_df(od = test_od, od_sp = test_od_sp2, config = test_config), 'od_sp is missing the necessary column origin')
  testthat::expect_error(create_departure_df(od = test_od, od_sp = test_od_sp3, config = test_config), 'od_sp is missing the necessary column destination')
  testthat::expect_error(create_departure_df(od = test_od, od_sp = test_od_sp4, config = test_config), 'od_sp is missing the necessary column shortest_path_length')

  test_config2 <- data.frame()
  test_config3 <- data.frame(CRITICAL_DISTANCE = 'abc')
  test_config4 <- data.frame(CRITICAL_DISTANCE = -5.0)

  testthat::expect_error(create_departure_df(od = test_od, od_sp = test_od_sp1, config = test_config2), 'config is missing the field `CRITICAL_DISTANCE`')
  testthat::expect_error(create_departure_df(od = test_od, od_sp = test_od_sp1, config = test_config3), 'CRITICAL_DISTANCE` should be of class numeric')
  testthat::expect_error(create_departure_df(od = test_od, od_sp = test_od_sp1, config = test_config4), '`CRITICAL_DISTANCE` should be a positive number')
})
