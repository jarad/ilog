context("min_value")

test_that("min_value throws an error", {
  expect_error(min_value(-1))
  expect_error(min_value(-1:2))
  expect_error(min_value(-1:-5))
})

test_that("min_value returns correct results", {
  expect_equal(min_value(1  ), 1)
  expect_equal(min_value(1:2), 1)
  expect_equal(min_value(0:2), 1)
})
