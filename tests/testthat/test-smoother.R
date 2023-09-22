## library(testthat)
## test_file("tests/testthat/test-smoother.R")

test_that("smoother function stops if 'x' is NULL or not a numeric vector", {
  expect_error(smoother(x = NULL, n = 0.2),
               regexp = "smoother: the 'x' input should be a numeric vector.")
  expect_error(smoother(x = "text", n = 0.2),
               regexp = "smoother: the 'x' input should be a numeric vector.")
  expect_error(smoother(x = 1, n = 0.2),
               regexp = "smoother: the 'x' input should be a numeric vector.")
})

test_that("smoother function stops if 'n' is not a single numeric", {
  expect_error(smoother(x = 1:10, n = NULL),
               regexp = "smoother: for 'movav' and 'loess' methods the 'n' input should be a single numeric value.")
  expect_error(smoother(x = 1:10, n = 1:2),
               regexp = "smoother: the 'n' input should be NULL or a single numeric value.")
})

test_that("smoother function stops if 'method' not recognised", {
  expect_error(smoother(x = 1:10, n = 0.2, method = "text"),
               regexp = "smoother: 'method' not recognised.")
})

test_that("smoother - method = 'movav' outputs correct results", {
  ## works
  expect_error(smoother(1:10, n = 0.2, method = "movav", plot = F),
                regexp = NA)
  ## correct length
  expect_equal(length(smoother(1:10, n = 0.2, method = "movav", plot = F)),
                10)
  ## correct results
  expect_equal(smoother(1:10, n = 0.2, method = "movav", plot = F),
               c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.0))
  ## accepts n as both n entries and proportion
  expect_equal(smoother(resp_noisy.rd, n = 0.3, method = "movav", plot = F),
               smoother(resp_noisy.rd, n = round(length(resp_noisy.rd)*0.3),
                      method = "movav", plot = F))
})


test_that("smoother - method = 'loess' outputs correct results", {
  ## works
  expect_error(smoother(resp_noisy.rd, n = 0.2, method = "loess", plot = T),
                regexp = NA)
  ## correct length
  expect_equal(length(smoother(resp_noisy.rd, n = 0.2, method = "loess", plot = F)),
                length(resp_noisy.rd))
  ## check a couple of exact values in case something changes output in future
  expect_equal(round(smoother(resp_noisy.rd, n = 0.2, method = "loess", plot = F)[c(1,935)], 6),
               c(8.173439,8.166100))
  ## accepts n as both n entries and proportion
  expect_equal(smoother(resp_noisy.rd, n = 0.2, method = "loess", plot = F),
               smoother(resp_noisy.rd, n = round(length(resp_noisy.rd)*0.2),
                      method = "loess", plot = F))
})


test_that("smoother - method = 'spline' outputs correct results", {
  ## works
  expect_error(smoother(resp_noisy.rd, n = 0.4, method = "spline", plot = T),
                regexp = NA)
  ## correct length
  expect_equal(length(smoother(resp_noisy.rd, n = 0.2, method = "spline", plot = F)),
                length(resp_noisy.rd))
  ## check a couple of exact values in case something changes output in future
  expect_equal(round(smoother(resp_noisy.rd, n = 0.2, method = "spline", plot = F)[c(1,935)], 6),
               c(8.169039, 8.165303))
  ## accepts n as both n entries and proportion
  expect_equal(smoother(resp_noisy.rd, n = 0.2, method = "spline", plot = F),
               smoother(resp_noisy.rd, n = round(length(resp_noisy.rd)*0.2),
                      method = "spline", plot = F))
})

test_that("smoother - method = 'supersmooth' outputs correct results", {
  ## works
  expect_error(smoother(resp_noisy.rd, n = 0.4, method = "supersmooth", plot = T),
                regexp = NA)
  ## correct length
  expect_equal(length(smoother(resp_noisy.rd, n = 0.2, method = "supersmooth", plot = F)),
                length(resp_noisy.rd))
  ## check a couple of exact values in case something changes output in future
  expect_equal(round(smoother(resp_noisy.rd, n = 0.02, method = "supersmooth", plot = F)[c(1,935)], 6),
               c(8.169576, 8.164984))
  ## accepts n as both n entries and proportion
  expect_equal(smoother(resp_noisy.rd, n = 0.2, method = "supersmooth", plot = F),
               smoother(resp_noisy.rd, n = round(length(resp_noisy.rd)*0.2),
                      method = "supersmooth", plot = F))
})



