#' @name run_linear_models
#' @title Fit a Linear or Mixed Effects Model with Optional Modifiers
#'
#' @description This function fits a linear model (`lm`) or a mixed effects model (`lmer`) depending on the presence of random effects. It supports interaction terms, sensitivity covariates, and returns a tidy summary of the model.
#'
#' @param data Your dataset containing the variables used in the model.
#' @param outcome A string specifying the outcome variable.
#' @param exposure A string specifying the main exposure variable.
#' @param covariates A character vector of covariate names to adjust for.
#' @param effect_modifier A string specifying an effect modifier to interact with the exposure.
#' @param sensitivity_cov A character vector of sensitivity covariates.
#' @param random_effects A string specifying the random effects structure (e.g., "(1 | group)").
#'
#' @return A list containing the fitted model, a tidy summary table, the model formula, residuals, and exposure name.
#' @export
#'
#' @importFrom utils globalVariables
#' @importFrom stats as.formula lm nobs BIC
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate select
#' @importFrom broom tidy augment
#' @importFrom lme4 lmer
#' @importFrom purrr safely
utils::globalVariables(c("term", "estimate", "conf.low", "conf.high", "std.error", "p.value"))

run_linear_models <- function(data, outcome, exposure, covariates = NULL, effect_modifier = NULL, sensitivity_cov = NULL, random_effects = NULL) {

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
  safe_fit <- safely(function(...) {
    withCallingHandlers(
      if (model_type == "lmer") lmer(...) else lm(...),
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

  tidy <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    dplyr::select(term, estimate, conf.low, conf.high, std.error, p.value) %>%
    mutate(error = ifelse(length(warnings) > 0 || length(messages) > 0, paste(c(warnings, messages), collapse = "; "), NA),
           n_obs = nobs(model),
           BIC = BIC(model))

  if (!is.null(effect_modifier)) {
    tidy <- tidy %>% mutate(modifier = effect_modifier)
  }

  if (!is.null(sensitivity_cov)) {
    tidy <- tidy %>% mutate(sensitivity = paste(sensitivity_cov, collapse = ", "))
  }

  augmented_data <- augment(model)

  list(model = model, tidy = tidy, formula = full_formula, residuals = augmented_data$.resid, exposure = exposure)
}
