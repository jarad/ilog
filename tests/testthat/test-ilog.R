context("ilog")

test_that("ilog throws an error", {
  # These come from min_value
  expect_error(ilog(-1))
  expect_error(ilog(-1:2))
  expect_error(ilog(-1:-5))

  # These come from ilog
  expect_error(ilog(-1,-1))
})

test_that("ilog throws a warning", {
  expect_warning(ilog(1))
  expect_warning(ilog(1:2))
})

test_that("ilog returns correct results", {
  expect_equal(suppressWarnings(ilog(1  )), log(1))
  expect_equal(suppressWarnings(ilog(1:2)), log(1:2))
  expect_equal(                 ilog(0:2) , log(0:2+1))
})
