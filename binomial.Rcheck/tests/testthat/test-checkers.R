context("Checkers")

test_that("testing for check_prob", {
  expect_true(check_prob(0.5))
  expect_error(check_prob(1:5))
  expect_error(check_prob("hello"))
})

test_that("testing for check_trials", {
  expect_true(check_trials(20))
  expect_error(check_trials(-34))
  expect_error(check_trials("hello"))
})

test_that("testing for check_success", {
  expect_true(check_success(20, 44))
  expect_error(check_success(44, 20))
  expect_error(check_success(0, 0))
})
