library(testthat)
library(callr)
library(here)

test_that("generate_assignment_reminder returns expected message", {
  msg <- generate_assignment_reminder("my_function")

  expect_true(any(grepl("Note: You ran `my_function` without assigning the result", msg)))
  expect_true(any(grepl("results <- my_function\\(\\.\\.\\.\\)", msg)))
  expect_true(any(grepl("results\\$tidy", msg)))
})

test_that("print_assignment_reminder does not print when called inside another function", {
  wrapper <- function() {
    capture.output(print_assignment_reminder("wrapped_function"))
  }
  output <- wrapper()
  expect_length(output, 0)
})

test_that("print_assignment_reminder prints message when forced", {
  expect_message(
    print_assignment_reminder(.test_force = TRUE),
    "Reminder: If you want to access the different outputs of your model later, don't forget to assign the result to a variable!"
  )
})
