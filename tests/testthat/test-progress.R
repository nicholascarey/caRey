## library(testthat)
## test_file("tests/testthat/test-progress.R")

test_that("progress function stops if 'x' is NULL or not numeric", {
  expect_error(progress(x = NULL, total = 10),
               regexp = "progress: valid numeric 'x' input not found.")
  expect_error(progress(x = "text", total = 10),
               regexp = "progress: valid numeric 'x' input not found.")
})

test_that("progress function stops if 'total' is NULL or not numeric", {
  expect_error(progress(x = 1, total = NULL),
               regexp = "progress: please enter a numeric 'total' input.")
  expect_error(progress(x = 1, total = "text"),
               regexp = "progress: please enter a numeric 'total' input.")
})

test_that("progress function message if 'x' is greater than 'total", {
  expect_message(progress(x = 11, total = 10),
               regexp = "progress: 'x' input greater than 'total' input.")
})

test_that("progress function outputs expected characters of start of progress bar in console", {
  expect_output(progress(1, total = 10),
                regexp = "\\[==")
})

test_that("progress function prints no input 'message' at end of progress bar if NULL", {
  expect_output(progress(1, total = 10, message = NULL),
                regexp = "]  10%")
})

test_that("progress function prints input 'message' at end of progress bar", {
  expect_output(progress(1, total = 10, message = "text"),
                regexp = "]  10% text")
})

test_that("progress function prints to the console", {
  expect_output(for(i in 1:10) progress(i, total = 10))
})

