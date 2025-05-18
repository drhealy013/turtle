library(testthat)

test_that("generate_assignment_reminder returns expected message", {
  # Default function name
  reminder <- generate_assignment_reminder()
  expect_type(reminder, "character")
  expect_true(any(grepl("Note: You ran `this function`", reminder)))
  expect_true(any(grepl("assign the output", reminder)))
  expect_true(any(grepl("results <- this function\\(\\.\\.\\.\\)", reminder)))
  expect_true(any(grepl("results\\$tidy", reminder)))

  # Custom function name
  reminder_custom <- generate_assignment_reminder("run_linear_models")
  expect_true(any(grepl("Note: You ran `run_linear_models`", reminder_custom)))
  expect_true(any(grepl("results <- run_linear_models\\(\\.\\.\\.\\)", reminder_custom)))
})
