% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_linear_models.R
\name{run_linear_models}
\alias{run_linear_models}
\title{Fit Linear or Mixed Effects Models Across Multiple Variable Combinations}
\arguments{
\item{data}{A data frame containing the variables used in the model.}

\item{outcome}{A character vector of outcome variable names.}

\item{exposure}{A character vector of exposure variable names.}

\item{covariates}{A character vector of covariate names to adjust for (e.g., confounders). These are included in all models.}

\item{effect_modifier}{A character vector of effect modifiers to interact with the exposure. All combinations will be tested.}

\item{sensitivity_cov}{A character vector of sensitivity covariates to include in the model. All combinations will be tested.}

\item{random_effects}{A string specifying the random effects structure (e.g., "(1 | group)"). If `NULL`, a linear model is used.}

\item{p_values}{Logical. If `TRUE` (default), uses `lmerTest` to compute p-values for mixed models. If `FALSE`, uses `lme4` without p-values.}

\item{verbose}{Logical. If `TRUE` (default), prints progress and model status messages.}

\item{return_grid}{Logical. If `TRUE`, attaches the model grid as an attribute to the returned object.}
}
\value{
A named list of model results. Each element contains:
\describe{
\item{model}{The fitted model object.}
\item{tidy}{A tidy summary of the model (term, estimate, confidence intervals, p-values, etc.).}
\item{residuals}{Model residuals.}
\item{formula}{The model formula used.}
}
If multiple combinations are tested, the list is named using the format `outcome&exposure&modifier&sensitivity`.

A list containing model results. If multiple outcomes or exposures are provided, returns a named list of results.
}
\description{
This function fits linear (`lm`) or mixed effects (`lmer`) models across combinations of outcomes, exposures, effect modifiers, and sensitivity covariates. It supports interaction terms, random effects, and returns a tidy summary of each model. It is designed to simplify model fitting across many variable combinations with consistent covariates and random effects.

For internal testing purposes, a hidden `.test_force` argument can be passed via `...` to simulate interactive, top-level calls. This enables reliable testing of preview and reminder output without relying on `interactive()` or `sys.nframe()`.
}
\note{
For internal testing purposes, a `.test_force` flag can be passed via the `.internal` argument (as a named list) to simulate interactive, top-level calls. This is used only in tests and should not be used in production.
}
\examples{
# Examples

data <- mtcars

\dontrun{
# 1. Run a single model with one outcome and one exposure
run_linear_models(data = data,
                  outcome = "mpg",
                  exposure = "cyl")
}

# 2. Store the output for a single model
model_output <- run_linear_models(data = data,
                                  outcome = "mpg",
                                  exposure = "cyl")
model_output$tidy  # access tidy results

# 3. Store output for multiple outcomes and exposures
multi_output <- run_linear_models(data = data,
                                  outcome = c("mpg", "hp"),
                                  exposure = c("cyl", "wt"))
multi_output[["mpg&cyl"]]$tidy  # access a specific model's tidy results

# 4. Use named vectors for outcomes and exposures
outcomes <- c("mpg", "hp")
exposures <- c("cyl", "wt")
named_output <- run_linear_models(data = data,
                                  outcome = outcomes,
                                  exposure = exposures)
named_output[["hp&wt"]]$tidy

# 5. Include covariates directly
covar_output <- run_linear_models(data = data,
                                  outcome = "mpg",
                                  exposure = "cyl",
                                  covariates = c("wt", "hp"))
covar_output$tidy

# 6. Include covariates using a named vector
covars <- c("wt", "hp")
names(covars) <- covars
named_covar_output <- run_linear_models(data = data,
                                        outcome = "mpg",
                                        exposure = "cyl",
                                        covariates = covars)
named_covar_output$tidy

# 7. Include an effect modifier (e.g., interaction term)
interaction_output <- run_linear_models(data = data,
                                        outcome = "mpg",
                                        exposure = "cyl",
                                        effect_modifier = "am")
interaction_output$tidy

# 8. Model with a non-existent variable (graceful failure)
error_output <- run_linear_models(data = data,
                                  outcome = "mpg",
                                  exposure = "not_a_column")

error_output$tidy

}
\keyword{internal}
