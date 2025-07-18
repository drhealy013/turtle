test_that("generate_index_page returns a grid grob when labels are provided", {
  labels <- c("Group A", "Group B", "Group C")
  grob <- generate_index_page(labels = labels, n_models = 30)

  expect_true(inherits(grob, "grob"))
  expect_true(inherits(grob, "text"))
})

test_that("generate_index_page returns NULL when labels are NULL", {
  grob <- generate_index_page(labels = NULL, n_models = 10)
  expect_null(grob)
})
