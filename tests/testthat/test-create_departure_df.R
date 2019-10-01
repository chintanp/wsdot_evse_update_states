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
      "origin" = 98001,
      "destination" = 98002,
      "shortest_path_length" = 100.56
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
