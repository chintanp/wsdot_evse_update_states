testthat::test_that("get_tripEVs_from_sourceEVs returns a dataframe with the required count", {
  test_trips_source_i1 <- 1
  test_source_EVs <- data.frame(
    a = c(1, 2, 4),
    b = c("a1", "b1", "c1")
  )
  test_trips_source_i2 <- 4
  expect_is(get_tripEVs_from_sourceEVs(trips_source_i = test_trips_source_i1,
                                       source_EVs = test_source_EVs), "data.frame")
  expect_equal(nrow(get_tripEVs_from_sourceEVs(trips_source_i = test_trips_source_i1,
                                               source_EVs = test_source_EVs)), 1)
  expect_equal(nrow(get_tripEVs_from_sourceEVs(trips_source_i = test_trips_source_i2,
                                               source_EVs = test_source_EVs)), 3)
})
