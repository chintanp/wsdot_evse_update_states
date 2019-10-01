testthat::test_that("get_Gas_Price returns a positive number", {
  test_gas_prices1 <- data.frame(zip = 98001, avg_gas_price = 4.56)
  testthat::expect_gt(get_Gas_Price(gas_prices = test_gas_prices1,
                                   origin_zip = 98001), 0.0)
  testthat::expect_is(get_Gas_Price(gas_prices = test_gas_prices1,
                                   origin_zip = 98001), 'numeric')
})
