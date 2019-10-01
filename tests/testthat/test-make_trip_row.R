testthat::test_that("make_trip_row returns a dataframe row with required columns", {

  test_gas_prices1 <- data.frame(zip = 98001, avg_gas_price = 4.56)
  test_trip_EV_row <- data.frame(connector_code = 1, origin_zip = 98001)
test_trip_cd <- data.frame(cd_chadem0 = 43.67, cd_combo = 67.89)
test_trip_sp <- data.frame(shortest_path_length = 234.56)
test_trip_dc <- data.frame(dc_combo = 1, dc_chademo = 0, )
  testthat::expect_is()
})
