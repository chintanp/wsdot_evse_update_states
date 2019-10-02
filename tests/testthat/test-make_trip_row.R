testthat::test_that("make_trip_row returns a dataframe row with required columns", {
  test_gas_prices1 <- data.frame(zip = 98001, avg_gas_price = 4.56)
  test_trip_EV_row1 <-
    data.frame(connector_code = 1, origin_zip = 98001)
  test_trip_cd1 <- data.frame(cd_chadem0 = 43.67, cd_combo = 67.89)
  test_trip_sp1 <- data.frame(shortest_path_length = 234.56)
  test_trip_dc1 <-
    data.frame(dc_combo = 1,
               dc_chademo = 0,
               dc_level2 = 0)

  testthat::expect_is(
    make_trip_row(
      gas_prices = test_gas_prices1,
      trip_EV_row = test_trip_EV_row1,
      trip_sp = test_trip_sp1,
      trip_cd = test_trip_cd1,
      trip_dc = test_trip_dc1
    ),
    "data.frame"
  )

  testthat::expect_equal(nrow(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row1,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc1
  )), 1)

  testthat::expect_setequal(colnames(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row1,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc1
  )), c('dist', 'dest_charger_L2', 'dest_charger', 'max_spacing', 'gas_price'))

  test_gas_prices2 <- data.frame(zip = 98001, avg_gas_price = 4.56)
  test_trip_EV_row2 <-
    data.frame(connector_code = 2, origin_zip = 98001)
  test_trip_cd2 <- data.frame()
  test_trip_sp2 <- data.frame(shortest_path_length = 234.56)
  test_trip_dc2 <-
    data.frame(dc_combo = 1,
               dc_chademo = 0,
               dc_level2 = 0)

  testthat::expect_is(
    make_trip_row(
      gas_prices = test_gas_prices2,
      trip_EV_row = test_trip_EV_row2,
      trip_sp = test_trip_sp2,
      trip_cd = test_trip_cd2,
      trip_dc = test_trip_dc2
    ),
    "data.frame"
  )

  testthat::expect_equal(nrow(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row2,
    trip_sp = test_trip_sp2,
    trip_cd = test_trip_cd2,
    trip_dc = test_trip_dc2
  )), 1)

  testthat::expect_setequal(colnames(make_trip_row(
    gas_prices = test_gas_prices2,
    trip_EV_row = test_trip_EV_row2,
    trip_sp = test_trip_sp2,
    trip_cd = test_trip_cd2,
    trip_dc = test_trip_dc2
  )), c('dist', 'dest_charger_L2', 'dest_charger', 'max_spacing', 'gas_price'))

  test_gas_prices3 <- data.frame(zip = 98001, avg_gas_price = 4.56)
  test_trip_EV_row3 <-
    data.frame(connector_code = 4, origin_zip = 98001)
  test_trip_cd3 <- data.frame(cd_chadem0 = 43.67, cd_combo = 67.89)
  test_trip_sp3 <- data.frame(shortest_path_length = 234.56)
  test_trip_dc3 <-
    data.frame(dc_combo = 1,
               dc_chademo = 0,
               dc_level2 = 0)

  testthat::expect_is(
    make_trip_row(
      gas_prices = test_gas_prices3,
      trip_EV_row = test_trip_EV_row3,
      trip_sp = test_trip_sp3,
      trip_cd = test_trip_cd3,
      trip_dc = test_trip_dc3
    ),
    "data.frame"
  )

  testthat::expect_equal(nrow(make_trip_row(
    gas_prices = test_gas_prices3,
    trip_EV_row = test_trip_EV_row3,
    trip_sp = test_trip_sp3,
    trip_cd = test_trip_cd3,
    trip_dc = test_trip_dc3
  )), 1)

  testthat::expect_setequal(colnames(make_trip_row(
    gas_prices = test_gas_prices3,
    trip_EV_row = test_trip_EV_row3,
    trip_sp = test_trip_sp3,
    trip_cd = test_trip_cd3,
    trip_dc = test_trip_dc3
  )), c('dist', 'dest_charger_L2', 'dest_charger', 'max_spacing', 'gas_price'))

})


testthat::test_that("make_trip_row gives the correct error on wrong inputs", {

  test_gas_prices1 <- data.frame(zip = 98001, avg_gas_price = 4.56)
  test_trip_EV_row1 <-
    data.frame(connector_code = 1, origin_zip = 98001)
  test_trip_cd1 <- data.frame(cd_chadem0 = 43.67, cd_combo = 67.89)
  test_trip_sp1 <- data.frame(shortest_path_length = 234.56)
  test_trip_dc1 <-
    data.frame(dc_combo = 1,
               dc_chademo = 0,
               dc_level2 = 0)
  test_gas_prices2 <- list(zip = 98001, avg_gas_price = 4.56)
  test_trip_EV_row2 <-
    list(connector_code = 1, origin_zip = 98001)
  test_trip_cd2 <- list(cd_chadem0 = 43.67, cd_combo = 67.89)
  test_trip_sp2 <- list(shortest_path_length = 234.56)
  test_trip_dc2 <-
    list(dc_combo = 1,
               dc_chademo = 0,
               dc_level2 = 0)

  testthat::expect_error(make_trip_row(
    gas_prices = test_gas_prices2,
    trip_EV_row = test_trip_EV_row1,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc1
  ), 'gas_prices should be of type data.frame')

  testthat::expect_error(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row2,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc1
  ), 'trip_EV_row should be of type data.frame')
  testthat::expect_error(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row1,
    trip_sp = test_trip_sp2,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc1
  ), 'trip_sp should be of type data.frame')
  testthat::expect_error(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row1,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd2,
    trip_dc = test_trip_dc1
  ), 'trip_cd should be of type data.frame')
  testthat::expect_error(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row1,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc2
  ), 'trip_dc should be of type data.frame')

  test_trip_EV_row3 <-
    data.frame(origin_zip = 98001)

  test_trip_EV_row4 <-
    data.frame(connector_code = "abc", origin_zip = 98001)

  test_trip_EV_row5 <-
    data.frame(connector_code = 1)

  test_trip_EV_row6 <-
    data.frame(connector_code = 1, origin_zip = "abc")

  testthat::expect_error(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row3,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc1
  ), 'trip_EV_row should have a column connector_code')

  testthat::expect_error(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row4,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc1
  ), 'column connector_code in trip_EV_row should be of type integer')

  testthat::expect_error(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row5,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc1
  ), 'trip_EV_row should have a column origin_zip')

  testthat::expect_error(make_trip_row(
    gas_prices = test_gas_prices1,
    trip_EV_row = test_trip_EV_row6,
    trip_sp = test_trip_sp1,
    trip_cd = test_trip_cd1,
    trip_dc = test_trip_dc1
  ), 'column origin_zip in trip_EV_row should be of type integer')


})
