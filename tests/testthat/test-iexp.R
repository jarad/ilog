context("iexp")

test_that("iexp throws a warning", {
  expect_warning(iexp(1))
  expect_warning(iexp(1:2))
})

test_that("iexp returns correct results", {
  expect_equal(iexp(1,   0), exp(1))
  expect_equal(iexp(1:2, 0), exp(1:2))

  expect_equal(iexp(1,   1), exp(1)-1)
  expect_equal(iexp(1:2, 1), exp(1:2)-1)

  expect_equal(suppressWarnings(iexp(1,   1)), exp(1)-1)
  expect_equal(suppressWarnings(iexp(1:2, 1)), exp(1:2)-1)

})
