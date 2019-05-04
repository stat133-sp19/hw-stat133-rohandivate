context("Summary Measures")

test_that("testing for aux_mean", {
  expect_equal(3, aux_mean(10, 0.3))
  expect_length(aux_mean(10, 0.4), 1)
  expect_type(aux_mean(100, 0.25), "double")
})

test_that("testing for aux_variance", {
  expect_equal(2.1, aux_variance(10, 0.3))
  expect_length(aux_variance(123,0.4), 1)
  expect_type(aux_variance(100,.5), "double")
})

test_that("testing for aux_mode", {
  expect_equal(3, aux_mode(10, 0.3))
  expect_length(aux_mode(33, 0.5), 2)
  expect_type(aux_variance(32, 0.5), "double")
})

test_that("testing for aux_skewness", {
  expect_equal(0.08512565, aux_skewness(23, 0.4))
  expect_length(aux_skewness(213, 0.8), 1)
  expect_type(aux_skewness(54, 0.3), "double")
})

test_that("testing for aux_kurtosis", {
  expect_equal(-0.04263566, aux_kurtosis(43, 0.4))
  expect_length(aux_kurtosis(84, 0.01), 1)
  expect_type(aux_kurtosis(94, 0.23), "double")
})
