context("Binomial Functions")

test_that("testing for bin_choose", {
  expect_equal(c(5, 10, 10), bin_choose(5, 1:3))
  expect_length(bin_choose(10, 3:4), 2)
  expect_type(bin_choose(100, 23), "double")
})

test_that("testing for bin_probability", {
  expect_equal(c(0.03125, 0.15625, 0.31250), bin_probability(success = 0:2, trials = 5, prob = 0.5))
  expect_length(bin_probability(1:2, 44, 0.1), 2)
  expect_type(bin_probability(13, 23, 0.5), "double")
})

test_that("testing for bin_distribution", {
  df <- data.frame(success = c(0, 1, 2), probability = c(0.25, 0.50, 0.25))
  class(df) <- c("bindis", "data.frame")
  expect_equal(df, bin_distribution(2, 0.5))
  expect_length(bin_distribution(3, 0.4), 2)
  expect_is(bin_distribution(34, 0.3), c("bindis", "data.frame"))
})

test_that("testing for bin_cumulative", {
  df <- data.frame(success = c(0, 1, 2), probability = c(0.49, 0.42, 0.09), cumulative = c(0.49, 0.91, 1.00))
  class(df) <- c("bincum", "data.frame")
  expect_equal(df, bin_cumulative(2, 0.3))
  expect_length(bin_cumulative(7, 0.4), 3)
  expect_is(bin_cumulative(34, 0.3), c("bincum", "data.frame"))
})

