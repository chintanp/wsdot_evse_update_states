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

testthat::test_that("get_tripEVs_from_sourceEVs raises the correct error on wrong inputs", {
  test_trips_source_i1 <- 1
  test_source_EVs1 <- list(
    a = c(1, 2, 4),
    b = c("a1", "b1", "c1")
  )
  expect_error(get_tripEVs_from_sourceEVs(trips_source_i = test_trips_source_i1,
                                       source_EVs = test_source_EVs1), "source_EVs should be of type data.frame")

  test_trips_source_i2 <- "abc"
  test_source_EVs <- data.frame(
    a = c(1, 2, 4),
    b = c("a1", "b1", "c1")
  )
  expect_error(get_tripEVs_from_sourceEVs(trips_source_i = test_trips_source_i2,
                                          source_EVs = test_source_EVs), "trips_source_i should be an integer or numeric value")

  test_trips_source_i3 <- -2
  expect_error(get_tripEVs_from_sourceEVs(trips_source_i = test_trips_source_i3,
                                          source_EVs = test_source_EVs), "trips_source_i should be a positive integer")
})
