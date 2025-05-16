#' @name run_linear_models
#' @title Fit a Linear or Mixed Effects Model with Optional Modifiers
#'
#' @description This function fits a linear model (`lm`) or a mixed effects model (`lmer`) depending on the presence of random effects. It supports interaction terms, sensitivity covariates, and returns a tidy summary of the model. Users can choose whether to compute p-values for mixed models. It also supports looping over multiple outcomes and exposures.
#'
#' @param data Your dataset containing the variables used in the model.
#' @param outcome A string or character vector specifying one or more outcome variables.
#' @param exposure A string or character vector specifying one or more exposure variables.
#' @param covariates A character vector of covariate names to adjust for.
#' @param effect_modifier A string specifying an effect modifier to interact with the exposure.
#' @param sensitivity_cov A character vector of sensitivity covariates.
#' @param random_effects A string specifying the random effects structure (e.g., "(1 | group)").
#' @param p_values Logical. If TRUE (default), uses lmerTest to compute p-values for mixed models. If FALSE, uses lme4 without p-values.
#'
#' @return A list containing model results. If multiple outcomes or exposures are provided, returns a named list of results.
#' @export
#'
#' @importFrom utils globalVariables packageVersion
#' @importFrom stats as.formula lm nobs BIC
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate select all_of
#' @importFrom broom tidy augment
#' @importFrom broom.mixed tidy
#' @importFrom lme4 lmer
#' @importFrom lmerTest lmer
#' @importFrom purrr safely pmap
#' @importFrom tidyr expand_grid

utils::globalVariables(c("term", "estimate", "conf.low", "conf.high", "std.error", "p.value"))

run_linear_models <- function(data, outcome, exposure, covariates = NULL,
                              effect_modifier = NULL, sensitivity_cov = NULL,
                              random_effects = NULL, p_values = TRUE) {

  # Handle multiple outcomes or exposures
  if (length(outcome) > 1 || length(exposure) > 1) {
    model_grid <- tidyr::expand_grid(outcome = outcome, exposure = exposure)
    results <- purrr::pmap(model_grid, function(outcome, exposure) {
      result <- run_linear_models(
        data = data,
        outcome = outcome,
        exposure = exposure,
        covariates = covariates,
        effect_modifier = effect_modifier,
        sensitivity_cov = sensitivity_cov,
        random_effects = random_effects,
        p_values = p_values
      )
      model_name <- paste(outcome, exposure, sep = "&")
      message("Stored model: ", model_name)
      list(model_name = model_name, model = result$model, tidy = result$tidy, residuals = result$residuals)
    })
    names(results) <- paste(model_grid$outcome, model_grid$exposure, sep = "&")
    return(results)
  }

  # Version check
  current_version <- utils::packageVersion("turtle")
  latest_version <- "0.1.4"
  if (current_version < latest_version) {
    message("A newer version of turtle is available (", latest_version,
            "). Please reinstall from GitHub to get the latest updates.")
  }

  # Build fixed effects part of the formula
  fixed_effects <- c(covariates, exposure)

  if (!is.null(effect_modifier) && grepl("\\*", exposure)) {
    if (interactive()) {
      message("You have specified an interaction term as the exposure (\"", exposure,
              "\") and also provided an effect modifier (\"", effect_modifier,
              "\"). This will result in a 3-way interaction: ",
              exposure, "*", effect_modifier, ".")

      choice <- menu(c("Yes", "No"), title = "Do you want to proceed?")
      if (choice != 1) {
        stop("Model fitting aborted by user.")
      }
    } else {
      stop("3-way interaction detected. Please confirm by setting confirm_three_way = TRUE.")
    }
  }

  if (!is.null(effect_modifier)) {
    fixed_effects <- c(fixed_effects, paste0(exposure, "*", effect_modifier))
  }

  if (!is.null(sensitivity_cov)) {
    fixed_effects <- c(fixed_effects, sensitivity_cov)
  }

  fixed_formula <- paste(outcome, "~", paste(fixed_effects, collapse = " + "))

  # Combine with random effects if provided
  if (!is.null(random_effects)) {
    full_formula <- as.formula(paste(fixed_formula, "+", random_effects))
    model_type <- "lmer"
  } else {
    full_formula <- as.formula(fixed_formula)
    model_type <- "lm"
  }

  # Safe model fitting
  safe_fit <- purrr::safely(function(...) {
    withCallingHandlers(
      if (model_type == "lmer") {
        if (p_values) {
          if (!requireNamespace("lmerTest", quietly = TRUE)) {
            stop("The 'lmerTest' package is required to compute p-values for mixed models. Please install it with install.packages('lmerTest').")
          }
          lmerTest::lmer(...)
        } else {
          lme4::lmer(...)
        }
      } else {
        lm(...)
      },
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  })

  warnings <- character()
  messages <- character()
  result <- safe_fit(formula = full_formula, data = data)

  if (!is.null(result$error)) {
    tidy <- tibble(term = NA, estimate = NA, conf.low = NA, conf.high = NA, p.value = NA, error = result$error$message)
    return(list(tidy = tidy, formula = full_formula, residuals = NA))
  }

  model <- result$result

  tidy_raw <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
  columns_to_select <- c("term", "estimate", "conf.low", "conf.high", "std.error")
  if ("p.value" %in% names(tidy_raw)) {
    columns_to_select <- c(columns_to_select, "p.value")
  }

  tidy <- tidy_raw %>%
    dplyr::select(dplyr::all_of(columns_to_select)) %>%
    dplyr::mutate(
      error = ifelse(length(warnings) > 0 || length(messages) > 0, paste(c(warnings, messages), collapse = "; "), NA),
      n_obs = nobs(model),
      BIC = BIC(model)
    )

  if (!is.null(effect_modifier)) {
    tidy <- tidy %>% dplyr::mutate(modifier = effect_modifier)
  }

  if (!is.null(sensitivity_cov)) {
    tidy <- tidy %>% dplyr::mutate(sensitivity = paste(sensitivity_cov, collapse = ", "))
  }

  augmented_data <- broom::augment(model)

  list(model = model, tidy = tidy, formula = full_formula, residuals = augmented_data$.resid, exposure = exposure)
}
