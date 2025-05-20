test_that("check_version_warning prints a message when current version is older", {
  expect_message(
    check_version_warning("0.1.0", latest_version = "0.1.6"),
    "A newer version of turtle is available"
  )
})

test_that("check_version_warning does not print when current version is up to date", {
  expect_silent(check_version_warning("0.1.6", latest_version = "0.1.6"))
  expect_silent(check_version_warning("0.2.0", latest_version = "0.1.6"))
})
