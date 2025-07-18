test_that("check_version_warning prints message when version is outdated (mocked)", {
  # Simulate installed version
  mock_version <- function(...) "0.1.0"

  # Simulate GitHub DESCRIPTION returning a newer version
  mock_dcf <- function(...) matrix("0.1.9", ncol = 1, dimnames = list(NULL, "Version"))

  mockery::stub(check_version_warning, "utils::packageVersion", mock_version)
  mockery::stub(check_version_warning, "read.dcf", mock_dcf)

  expect_message(check_version_warning(), "newer version of turtle is available")
})

test_that("check_version_warning is silent when up to date (mocked)", {
  mock_version <- function(...) "0.1.9"
  mock_dcf <- function(...) matrix("0.1.9", ncol = 1, dimnames = list(NULL, "Version"))

  mockery::stub(check_version_warning, "utils::packageVersion", mock_version)
  mockery::stub(check_version_warning, "read.dcf", mock_dcf)

  expect_silent(check_version_warning())
})

test_that("check_version_warning warns gracefully if GitHub fetch fails", {
  mock_version <- function(...) "0.1.0"
  mock_dcf <- function(...) stop("GitHub unreachable")

  mockery::stub(check_version_warning, "utils::packageVersion", mock_version)
  mockery::stub(check_version_warning, "read.dcf", mock_dcf)

  expect_warning(check_version_warning(), "Could not determine version information")
})
