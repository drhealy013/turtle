test_that("build_formula constructs correct formulas", {

  f1 <- build_formula("y", "x", c("a", "b"), NULL, NULL, NULL)
  expect_equal(deparse(f1), deparse(as.formula("y ~ a + b + x")))

  f2 <- build_formula("y", "x", c("a", "b"), "z", NULL, NULL)
  expect_equal(deparse(f2), deparse(as.formula("y ~ a + b + x + x*z")))

  f3 <- build_formula("y", "x", c("a", "b"), NULL, c("s1", "s2"), NULL)
  expect_equal(deparse(f3), deparse(as.formula("y ~ a + b + x + s1 + s2")))

  f4 <- build_formula("y", "x", c("a", "b"), "z", c("s1", "s2"), NULL)
  expect_equal(deparse(f4), deparse(as.formula("y ~ a + b + x + x*z + s1 + s2")))

  f5 <- build_formula("y", "x", c("a", "b"), NULL, NULL, "(1 | group)")
  expect_equal(deparse(f5), deparse(as.formula("y ~ a + b + x + (1 | group)")))
})
