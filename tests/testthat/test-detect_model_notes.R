library(testthat)

test_that("detect_model_notes detects dropped terms in lm", {
  data <- mtcars
  data$qsec_dup <- data$qsec

  model <- lm(mpg ~ wt + qsec + qsec_dup, data = data)
  note <- detect_model_notes(model)

  expect_type(note, "character")
  expect_true(grepl("Dropped due to collinearity", note))
  expect_true(grepl("qsec_dup", note))
})

test_that("detect_model_notes detects singularity in lmer", {
  skip_if_not_installed("lme4")
  library(lme4)

  data(sleepstudy, package = "lme4")

  sleepstudy$Group <- rep(1:3, length.out = nrow(sleepstudy))
  model <- lmer(Reaction ~ Days + (Days | Group), data = sleepstudy)

  note <- detect_model_notes(model)

  expect_type(note, "character")
  expect_true(grepl("singular", tolower(note)))
})

test_that("detect_model_notes returns NA when no issues", {
  model <- lm(mpg ~ wt + qsec, data = mtcars)
  note <- detect_model_notes(model)

  expect_true(is.na(note))
})
