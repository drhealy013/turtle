test_that("generate_summary_page returns a grid grob", {
  diagnostics <- c("outliers", "normality", "collinearity")
  grob <- generate_summary_page(
    n_models = 5,
    flagged_n = 2,
    diagnostics = diagnostics,
    outlier_threshold = 10,
    pdf_path = "diagnostics.pdf",
    readme_url = "https://yourproject.org/readme"
  )

  expect_true(inherits(grob, "grob"))
  expect_true(inherits(grob, "text"))
})
