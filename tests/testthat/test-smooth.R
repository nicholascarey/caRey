## library(testthat)
## test_file("tests/testthat/test-smooth.R")

test_that("smooth function stops if 'x' is NULL or not a numeric vector", {
  expect_error(smooth(x = NULL, n = 0.2),
               regexp = "smooth: the 'x' input should be a numeric vector.")
  expect_error(smooth(x = "text", n = 0.2),
               regexp = "smooth: the 'x' input should be a numeric vector.")
  expect_error(smooth(x = 1, n = 0.2),
               regexp = "smooth: the 'x' input should be a numeric vector.")
})

test_that("smooth function stops if 'n' is not a single numeric", {
  expect_error(smooth(x = 1:10, n = NULL),
               regexp = "smooth: for 'movav' and 'loess' methods the 'n' input should be a single numeric value.")
  expect_error(smooth(x = 1:10, n = 1:2),
               regexp = "smooth: the 'n' input should be NULL or a single numeric value.")
})

test_that("smooth function stops if 'method' not recognised", {
  expect_error(smooth(x = 1:10, n = 0.2, method = "text"),
               regexp = "smooth: 'method' not recognised.")
})

test_that("smooth - method = 'movav' outputs correct results", {
  ## works
  expect_error(smooth(1:10, n = 0.2, method = "movav", plot = F),
                regexp = NA)
  ## correct length
  expect_equal(length(smooth(1:10, n = 0.2, method = "movav", plot = F)),
                10)
  ## correct results
  expect_equal(smooth(1:10, n = 0.2, method = "movav", plot = F),
               c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.0))
  ## accepts n as both n entries and proportion
  expect_equal(smooth(resp_noisy.rd, n = 0.3, method = "movav", plot = F),
               smooth(resp_noisy.rd, n = round(length(resp_noisy.rd)*0.3),
                      method = "movav", plot = F))
})


test_that("smooth - method = 'loess' outputs correct results", {
  ## works
  expect_error(smooth(resp_noisy.rd, n = 0.2, method = "loess", plot = T),
                regexp = NA)
  ## correct length
  expect_equal(length(smooth(resp_noisy.rd, n = 0.2, method = "loess", plot = F)),
                length(resp_noisy.rd))
  ## check a couple of exact values in case something changes output in future
  expect_equal(round(smooth(resp_noisy.rd, n = 0.2, method = "loess", plot = F)[c(1,935)], 6),
               c(8.173439,8.166100))
  ## accepts n as both n entries and proportion
  expect_equal(smooth(resp_noisy.rd, n = 0.2, method = "loess", plot = F),
               smooth(resp_noisy.rd, n = round(length(resp_noisy.rd)*0.2),
                      method = "loess", plot = F))
})


