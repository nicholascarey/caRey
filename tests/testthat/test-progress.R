## library(testthat)
## test_file("tests/testthat/test-progress.R")

test_that("progress function stops if 'x' is NULL or not numeric", {
  expect_error(progress(x = NULL, max = 10),
               regexp = "progress: valid numeric 'x' input not found.")
  expect_error(progress(x = "text", max = 10),
               regexp = "progress: valid numeric 'x' input not found.")
})

test_that("progress function stops if 'max' is NULL or not numeric", {
  expect_error(progress(x = 1, max = NULL),
               regexp = "progress: please enter a numeric 'max' input.")
  expect_error(progress(x = 1, max = "text"),
               regexp = "progress: please enter a numeric 'max' input.")
})

test_that("progress function stops if 'x' is greater than 'max", {
  expect_error(progress(x = 11, max = 10),
               regexp = "progress: 'x' input greater than 'max' input.")
})

test_that("progress function outputs expected characters of start of progress bar in console", {
  expect_output(progress(1, max = 10),
                regexp = "\\[==")
})

test_that("progress function prints no input 'message' at end of progress bar if NULL", {
  expect_output(progress(1, max = 10, message = NULL),
                regexp = "] 10%")
})

test_that("progress function prints input 'message' at end of progress bar", {
  expect_output(progress(1, max = 10, message = "text"),
                regexp = "] 10% text")
})

test_that("progress function prints to the console", {
  expect_output(for(i in 1:10) progress(i, max = 10))
})
