#' @name plot_model_diagnostics
#' @title Generate Diagnostic Plots for a Single Model
#'
#' @description
#' This internal helper function generates diagnostic plots for a single fitted model (`lm` or `lmerMod`) based on the selected diagnostics. It returns a combined plot object along with the percentage of outliers detected (if applicable).
#'
#' Used internally by `run_model_diagnostics()` to process each model individually.
#'
#' @param model A fitted model object of class `lm` or `lmerMod`.
#' @param model_name A string used to label the model in the plot title.
#' @param diagnostics A character vector specifying which diagnostics to run. Options include `"heteroscedasticity"`, `"outliers"`, `"collinearity"`, `"normality"`, `"autocorrelation"`, `"random_effects"`.
#'
#' @return A list with:
#' \describe{
#'   \item{plot}{A combined `grid` object of diagnostic plots.}
#'   \item{percentage_outliers}{Numeric value indicating the percentage of outliers (if applicable).}
#'   \item{model}{The original model object.}
#' }
#'
#' @keywords internal diagnostics

plot_model_diagnostics <- function(model, model_name, diagnostics) {
  common_theme <- ggplot2::theme(
    plot.margin = ggplot2::margin(5, 5, 5, 5),
    plot.title = ggplot2::element_text(size = 10)
  )
  plots <- list()
  pct_outliers <- NA
  resid <- stats::residuals(model)

  if ("heteroscedasticity" %in% diagnostics) {
    plots$heteroscedasticity <- performance::check_heteroscedasticity(model) |>
      plot() + ggplot2::ggtitle("Heteroscedasticity") + common_theme
  }

  if ("outliers" %in% diagnostics) {
    outliers <- performance::check_outliers(model)
    plots$outliers <- plot(outliers) + ggplot2::ggtitle("Outliers") + common_theme
    outlier_tbl <- tibble::as_tibble(outliers)
    num_outliers <- sum(outlier_tbl$Outlier == 1)
    pct_outliers <- (num_outliers / nrow(outlier_tbl)) * 100
  }

  if ("collinearity" %in% diagnostics) {
    plots$collinearity <- performance::check_collinearity(model) |>
      plot() + ggplot2::ggtitle("Collinearity") + common_theme +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  if ("normality" %in% diagnostics) {
    plots$normality <- ggplot2::ggplot(data.frame(resid = resid), ggplot2::aes(sample = resid)) +
      ggplot2::stat_qq() + ggplot2::stat_qq_line() +
      ggplot2::ggtitle("Normality of Residuals") + common_theme
  }

  if ("autocorrelation" %in% diagnostics) {
    acf_data <- stats::acf(resid, plot = FALSE)
    conf <- 1.96 / sqrt(length(resid))
    plots$autocorrelation <- ggplot2::ggplot(
      data.frame(lag = acf_data$lag, acf = acf_data$acf),
      ggplot2::aes(x = lag, y = acf)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_hline(yintercept = c(conf, -conf), linetype = "dashed", color = "red") +
      ggplot2::ggtitle("Autocorrelation of Residuals") + common_theme
  }

  if ("random_effects" %in% diagnostics && inherits(model, "lmerMod")) {
    re <- lme4::ranef(model)
    re_data <- do.call(rbind, lapply(names(re), function(var) {
      data.frame(effect = unlist(re[[var]]), group = var)
    }))
    plots$random_effects <- ggplot2::ggplot(re_data, ggplot2::aes(sample = effect)) +
      ggplot2::stat_qq() + ggplot2::stat_qq_line() +
      ggplot2::ggtitle("Random Effects Distribution") + common_theme +
      ggplot2::facet_wrap(~ group)
  }

  title_grob <- grid::textGrob(paste("Diagnostics for Model:", model_name),
                               gp = grid::gpar(fontsize = 14, fontface = "bold"))
  combined_plot <- gridExtra::grid.arrange(
    title_grob,
    gridExtra::arrangeGrob(grobs = plots, ncol = 2),
    nrow = 2,
    heights = c(1, 10)
  )

  list(plot = combined_plot, percentage_outliers = pct_outliers, model = model)
}

#' @name generate_summary_page
#' @title Create Summary Page for Diagnostic PDF
#'
#' @description
#' Generates a summary page for the diagnostic PDF report. The summary includes a count of models processed, a list of diagnostics performed with plain-language explanations, and a link to further documentation.
#'
#' This function is used internally by `run_model_diagnostics()` when `save_pdf = TRUE`.
#'
#' @param n_models Integer. Total number of models processed.
#' @param flagged_n Integer. Number of models exceeding the outlier threshold.
#' @param diagnostics Character vector of diagnostics that were run.
#' @param outlier_threshold Numeric. Threshold used to flag models based on outlier percentage.
#' @param pdf_path String. Path where the PDF is saved.
#' @param readme_url String. URL to the online documentation or README.
#'
#' @return A `grid` text grob object to be rendered in the PDF.
#'
#' @keywords internal diagnostics

generate_summary_page <- function(n_models, flagged_n, diagnostics, outlier_threshold, pdf_path, readme_url) {
  diagnostic_explanations <- list(
    heteroscedasticity = "Whether the model’s errors are evenly spread or vary unpredictably.",
    outliers = "Whether any data points are unusually far from the model’s predictions.",
    collinearity = "Whether some predictors are too similar to each other.",
    normality = "Whether the model’s errors follow a bell-shaped curve.",
    autocorrelation = "Whether the model’s errors are related across time or sequence.",
    random_effects = "Whether the random effects (in mixed models) are normally distributed."
  )

  text <- paste0(
    "✅ Model Diagnostics Completed\n\n",
    "You’ve successfully run diagnostics on ", n_models, " models using the following checks:\n\n",
    paste0(
      sprintf("• %s: %s", names(diagnostic_explanations[diagnostics]), diagnostic_explanations[diagnostics]),
      collapse = "\n"
    ),
    "\n\n📊 Models with >", outlier_threshold, "% outliers: ", flagged_n, "\n",
    "📂 PDF saved to: ", pdf_path, "\n\n",
    "🔍 For more details, visit the online guide: ", readme_url
  )

  grid::textGrob(text, gp = grid::gpar(fontsize = 12), just = "left", x = 0.05)
}

#' @name generate_index_page
#' @title Create Index Page for Diagnostic PDF
#'
#' @description
#' Generates an optional index page for the diagnostic PDF, grouping models by user-defined labels. Each label is assigned a page range based on the number of models and total labels.
#'
#' This function is used internally by `run_model_diagnostics()` when `index_labels` are provided.
#'
#' @param labels A character vector of group labels (e.g., model categories or study phases).
#' @param n_models Integer. Total number of models processed.
#'
#' @return A `grid` text grob object to be rendered in the PDF.
#'
#' @keywords internal diagnostics

generate_index_page <- function(labels, n_models) {
  if (is.null(labels)) return(NULL)

  index_text <- "Index Page\n\n"
  page_number <- 2
  for (label in labels) {
    start_page <- page_number
    end_page <- page_number + n_models - 1
    index_text <- paste0(index_text, label, ": ", start_page, "-", end_page, "\n")
    page_number <- end_page + 1
  }

  grid::textGrob(index_text, gp = grid::gpar(fontsize = 12), just = "left", x = 0.05)
}

