# turtle (development version)

* Initial CRAN submission.

# runLinearModels 0.1.0

## Initial release

* Introduced `run_linear_models()` function to fit linear (`lm`) and mixed effects (`lmer`) models.
* Supports:
  - Covariate adjustment
  - Effect modification via interaction terms
  - Sensitivity covariates
  - Random effects specification
* Returns a comprehensive output including:
  - The fitted model object
  - A tidy summary of fixed effects with estimates, confidence intervals, standard errors, p-values, number of observations, and BIC
  - Model formula
  - Residuals
  - Exposure variable name
* Includes robust error and warning handling using `purrr::safely()` and `withCallingHandlers()`. Includes errors or warnings in the tidy output so they can be viewed later when interpreting results.

# runLinearModels 0.1.1

## Update

*Ask for clarification if user submits an interaction as the exposure, and additionally adds an effect-modifier, to confirm if the user wants to pursue 3-way interactions.
  - Robust for both interactive and non-interactive use
