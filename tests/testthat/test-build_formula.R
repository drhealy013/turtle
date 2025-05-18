library(testthat)

test_that("build_formula constructs correct formulas", {

  # No effect modifier or sensitivity covariates
  f1 <- build_formula("y", "x", c("a", "b"), NULL, NULL, NULL)
  expect_equal(deparse(f1), deparse(as.formula("y ~ a + b + x")))

  # With effect modifier (interaction replaces exposure)
  f2 <- build_formula("y", "x", c("a", "b"), "z", NULL, NULL)
  expect_equal(deparse(f2), deparse(as.formula("y ~ a + b + x*z")))

  # With sensitivity covariates
  f3 <- build_formula("y", "x", c("a", "b"), NULL, c("s1", "s2"), NULL)
  expect_equal(deparse(f3), deparse(as.formula("y ~ a + b + x + s1 + s2")))

  # With effect modifier and sensitivity covariates
  f4 <- build_formula("y", "x", c("a", "b"), "z", c("s1", "s2"), NULL)
  expect_equal(deparse(f4), deparse(as.formula("y ~ a + b + x*z + s1 + s2")))

  # With random effects
  f5 <- build_formula("y", "x", c("a", "b"), NULL, NULL, "(1 | group)")
  expect_equal(deparse(f5), deparse(as.formula("y ~ a + b + x + (1 | group)")))

  # Multiple outcomes: should warn and use first
  expect_warning({
    f6 <- build_formula(c("y1", "y2"), "x", c("a", "b"), NULL, NULL, NULL)
    expect_equal(deparse(f6), deparse(as.formula("y1 ~ a + b + x")))
  }, "Multiple outcomes supplied")

  # Multiple exposures: should warn and use first
  expect_warning({
    f7 <- build_formula("y", c("x1", "x2"), c("a", "b"), NULL, NULL, NULL)
    expect_equal(deparse(f7), deparse(as.formula("y ~ a + b + x1")))
  }, "Multiple exposures supplied")
})
