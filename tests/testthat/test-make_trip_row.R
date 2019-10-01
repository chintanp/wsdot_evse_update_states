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

})
