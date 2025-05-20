test_that("print_assignment_reminder prints full message when forced", {
  output <- capture.output(print_assignment_reminder("my_function", .test_force = TRUE))
  output <- cli::ansi_strip(output)

  expect_true(any(grepl("NOTE", output)))
  expect_true(any(grepl("without assigning the result", output)))
  expect_true(any(grepl("results\\s*<-\\s*my_function\\(", output)))
})

test_that("print_assignment_reminder infers function name when not provided", {
  output <- withr::with_options(
    list(cli.default_output = "message"),
    capture.output({
      wrapper <- function() {
        print_assignment_reminder(.test_force = TRUE)
      }
      wrapper()
    })
  )
  output <- cli::ansi_strip(output)

  cat("\n--- Captured Output ---\n", paste(output, collapse = "\n"), "\n------------------------")

  expect_true(any(grepl("You ran `wrapper\\(\\)` without assigning the result", output)))
  expect_true(any(grepl("results\\s*<-\\s*wrapper\\(", output)))
})
