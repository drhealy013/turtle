test_that("make_model_name constructs names correctly", {
  # Basic outcome and exposure
  expect_equal(make_model_name("y", "x"), "y&x")

  # With effect modifier
  expect_equal(make_model_name("y", "x", "z"), "y&x&z")

  # With NA effect modifier
  expect_equal(make_model_name("y", "x", NA), "y&x")

  # With NULL effect modifier
  expect_equal(make_model_name("y", "x", NULL), "y&x")

  # With numeric inputs
  expect_equal(make_model_name("out", 123, "mod"), "out&123&mod")

  # With empty strings
  expect_equal(make_model_name("", "x", "z"), "&x&z")
})
