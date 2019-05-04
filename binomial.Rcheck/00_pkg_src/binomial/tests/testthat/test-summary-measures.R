context("Summary Measures")

test_that("testing for aux_mean", {
  expect_equal(3, aux_mean(10, 0.3))
  expect_error(aux_mean(1:5))
  expect_error(aux_mean("hello"))
})

# test_that("testing for aux_variance", {
#   expect_true(check_prob(0.5))
#   expect_error(check_prob(1:5))
#   expect_error(check_prob("hello"))
# })
#
# test_that("testing for aux_mode", {
#   expect_true(aux_mode(0.5))
#   expect_error(aux_mode(1:5))
#   expect_error(aux_mode("hello"))
# })
#
# test_that("testing for aux_skewness", {
#   expect_true(aux_skewness(0.5))
#   expect_error(aux_skewness(1:5))
#   expect_error(aux_skewness("hello"))
# })
#
# test_that("testing for aux_kurtosis", {
#   expect_true(aux_kurtosis(0.5))
#   expect_error(aux_kurtosis(1:5))
#   expect_error(aux_kurtosis("hello"))
# })
