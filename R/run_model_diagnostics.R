#' @name run_model_diagnostics
#' @title Run Diagnostics on Fitted Linear or Mixed Effects Models
#'
#' @description
#' This function runs a suite of diagnostic checks on one or more fitted models (`lm` or `lmerMod`). It supports parallel processing, customizable diagnostic selection, and optional PDF export with index and summary pages. Designed to help users assess model quality and identify potential issues such as outliers, heteroscedasticity, multicollinearity, and more.
#'
#' A summary page provides layperson-friendly explanations of each diagnostic. Outlier percentages are calculated for each model and reported, but no models are removed or filtered based on these values.
#'
#' Parallel processing uses `future::multisession()` and automatically resets the plan after execution to avoid side effects.
#'
#' @param models A single fitted model (`lm` or `lmerMod`) or a named list of such models.
#' @param diagnostics A character vector specifying which diagnostics to run. Options include `"heteroscedasticity"`, `"outliers"`, `"collinearity"`, `"normality"`, `"autocorrelation"`, `"random_effects"`. Defaults to all.
#' @param parallel Logical. If `TRUE`, diagnostics are run in parallel using `furrr`. Defaults to `FALSE`.
#' @param outlier_threshold Numeric. Percentage threshold used for reporting (not filtering). Defaults to `10`.
#' @param save_pdf Logical. If `TRUE`, saves diagnostic plots to a PDF. Defaults to `FALSE`.
#' @param pdf_path String. File path for saving the PDF if `save_pdf = TRUE`. Defaults to `"model_diagnostics.pdf"`.
#' @param index_labels Optional character vector of labels for grouping models in the index page.
#' @param chunk_size Integer. Number of models per chunk for parallel processing. Defaults to `100`.
#' @param readme_url String. URL to the online guide or README for further explanation. Defaults to `"https://yourproject.org/readme"`.
#'
#' @return An object of class `"model_diagnostics_result"` containing:
#' \describe{
#'   \item{plots}{A list of diagnostic plots for each model.}
#'   \item{all_results}{A list of diagnostic results, including plots and outlier percentages.}
#' }
#'
#' @examples
#' \dontrun{
#' # Multiple models
#' models <- list(
#'   lm1 = lm(mpg ~ cyl + wt, data = mtcars),
#'   lm2 = lm(hp ~ wt + qsec, data = mtcars)
#' )
#' run_model_diagnostics(models,
#'                       diagnostics = c("outliers", "normality"),
#'                       parallel = FALSE,
#'                       save_pdf = TRUE,
#'                       pdf_path = "diagnostics_output.pdf")
#'
#' # Single model
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' run_model_diagnostics(model, diagnostics = c("normality", "outliers"))
#' }
#'
#' @seealso \code{\link{plot_model_diagnostics}}, \code{\link{generate_summary_page}}, \code{\link{generate_index_page}}
#'
#' @export
#'
#' @importFrom performance check_outliers check_heteroscedasticity check_collinearity
#' @importFrom ggplot2 ggplot aes geom_bar geom_hline stat_qq stat_qq_line ggtitle theme element_text margin facet_wrap
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom grid textGrob gpar grid.draw grid.newpage
#' @importFrom grDevices pdf dev.off
#' @importFrom stats residuals acf
#' @importFrom tibble as_tibble
#' @importFrom purrr map flatten
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multisession availableCores
#' @importFrom cli cli_alert_success cli_alert_info
#'
#' @keywords diagnostics models


run_model_diagnostics <- function(
    models,
    diagnostics = c("heteroscedasticity", "outliers", "collinearity", "normality", "autocorrelation", "random_effects"),
    parallel = FALSE,
    outlier_threshold = 10,
    save_pdf = FALSE,
    pdf_path = "model_diagnostics.pdf",
    index_labels = NULL,
    chunk_size = 100,
    readme_url = "https://github.com/drhealy013/turtle"
) {
  # Handle single model input
  if (inherits(models, "lm") || inherits(models, "lmerMod")) {
    models <- list(model1 = models)
  }

  # Validate input
  if (!is.list(models) || is.null(names(models))) {
    stop("`models` must be a named list or a single model object.")
  }

  # Parallel safety
  if (parallel) {
    oplan <- future::plan()
    on.exit(future::plan(oplan), add = TRUE)
    future::plan(future::multisession, workers = future::availableCores() - 1)
  }

  # Chunking
  split_into_chunks <- function(lst, size) split(lst, ceiling(seq_along(lst) / size))
  model_chunks <- split_into_chunks(models, chunk_size)

  # Run diagnostics
  run_chunk <- function(chunk) {
    if (parallel) {
      furrr::future_map(names(chunk), ~ plot_model_diagnostics(chunk[[.x]], .x, diagnostics))
    } else {
      purrr::map(names(chunk), ~ plot_model_diagnostics(chunk[[.x]], .x, diagnostics))
    }
  }

  diagnostic_results <- purrr::flatten(purrr::map(model_chunks, run_chunk))
  diagnostic_plots <- purrr::map(diagnostic_results, "plot")

  # Count flagged models
  n_flagged <- sum(purrr::map_dbl(diagnostic_results, ~ {
    pct <- .x$percentage_outliers
    if (!is.na(pct) && pct > outlier_threshold) 1 else 0
  }))

  # Save PDF
  if (save_pdf) {
    grDevices::pdf(pdf_path, width = 14, height = 10)
    grid::grid.draw(generate_summary_page(length(models), n_flagged, diagnostics, outlier_threshold, pdf_path, readme_url))
    grid::grid.newpage()
    if (!is.null(index_labels)) {
      grid::grid.draw(generate_index_page(index_labels, length(models)))
      grid::grid.newpage()
    }
    for (p in diagnostic_plots) {
      grid::grid.draw(p)
      grid::grid.newpage()
    }
    grDevices::dev.off()
  }

  cli::cli_alert_success("Model diagnostics completed for {length(models)} models.")
  cli::cli_alert_info("Models with >{outlier_threshold}% outliers: {n_flagged}")
  cli::cli_alert_info("PDF saved to: {ifelse(save_pdf, pdf_path, 'Not saved')}")
  cli::cli_alert_info("More info: {readme_url}")

  structure(
    list(plots = diagnostic_plots, all_results = diagnostic_results),
    class = "model_diagnostics_result"
  )
}
