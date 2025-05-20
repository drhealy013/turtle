test_that("generate_assignment_reminder returns expected message lines", {
  reminder <- cli::ansi_strip(generate_assignment_reminder())
  reminder_custom <- cli::ansi_strip(generate_assignment_reminder("run_linear_models"))

  expect_true(any(grepl("You ran `this function\\(\\)` without assigning the result", reminder)))
  expect_true(any(grepl("results <- this function\\(\\.\\.\\.\\)", reminder)))

  expect_true(any(grepl("You ran `run_linear_models\\(\\)` without assigning the result", reminder_custom)))
  expect_true(any(grepl("results <- run_linear_models\\(\\.\\.\\.\\)", reminder_custom)))
})
