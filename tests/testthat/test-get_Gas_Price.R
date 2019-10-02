testthat::test_that("get_Gas_Price returns a positive number", {
  test_gas_prices1 <- data.frame(zip = 98001, avg_gas_price = 4.56)
  testthat::expect_gt(get_Gas_Price(gas_prices = test_gas_prices1,
                                   origin_zip = 98001), 0.0)
  testthat::expect_is(get_Gas_Price(gas_prices = test_gas_prices1,
                                   origin_zip = 98001), 'numeric')
})


testthat::test_that("get_Gas_Price raises the correct error on wrong input", {

  test_gas_prices1 <- list(zip = 98001, avg_gas_price = 4.56)

  testthat::expect_error(get_Gas_Price(gas_prices = test_gas_prices1,
                                       origin_zip = 98001), 'gas_prices should be of type data.frame')

  test_gas_prices <- data.frame(zip = 98001, avg_gas_price = 4.56)

  testthat::expect_error(get_Gas_Price(gas_prices = test_gas_prices,
                                       origin_zip = "abc"), 'origin_zip should be an integer or numeric value')
  testthat::expect_error(get_Gas_Price(gas_prices = test_gas_prices,
                                       origin_zip = -3), 'origin_zip should be a positive integer')


})
