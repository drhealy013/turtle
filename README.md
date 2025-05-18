
# turtle

<!-- badges: start -->

[![R-CMD-check](https://github.com/drhealy013/turtle/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/drhealy013/turtle/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/drhealy013/turtle/branch/main/graph/badge.svg)](https://codecov.io/gh/drhealy013/turtle)
[![pkgdown](https://github.com/drhealy013/turtle/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/drhealy013/turtle/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

## Package Overview

`turtle` is an R package that provides simplified, user-friendly
functions for common data analysis tasks such as modelling,
dimensionality reduction, and visualization. It is designed to support
users with minimal programming knowledge by allowing them to specify key
inputs (e.g., outcome, exposure, covariates etc.) and automatically
handle the underlying code. It is applicable to a wide range of data
types, including epidemiological, ecological, and experimental datasets.
The core functions include:

- `run_linear_models()`: Fit linear or mixed models (can be run for a
  single model or many models).

## How to get the package

You can install the development version of `turtle` from GitHub with:

``` r
# install.packages("pak")
pak::pak("drhealy013/turtle")
```

## Workflow

Here is an example of how the different functions can be used in your
data analysis:

For more detail on each of the different sections of your data analysis,
you can find relevant guides at the end under the “Learn More” section.

## Quick Start

Here’s a basic example of the `run_linear_models()` using the built-in
`mtcars` dataset:

``` r
library(turtle)

# Example: Does car weight affect fuel efficiency?
result <- run_linear_models(
  data = mtcars,
  outcome = "mpg",   # miles per gallon
  exposure = "wt"    # weight of the car
)

# View the results
result$tidy
#> # A tibble: 2 × 9
#>   term        estimate conf.low conf.high std.error  p.value error n_obs   BIC
#>   <chr>          <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <lgl> <int> <dbl>
#> 1 (Intercept)    37.3     33.5      41.1      1.88  8.24e-19 NA       32  170.
#> 2 wt             -5.34    -6.49     -4.20     0.559 1.29e-10 NA       32  170.
```

The function also takes lists of exposures and/or outcomes and loops
through the different combinations. These can be provided directly to
the function, or by storing them in a variable.

``` r
library(turtle)

outcomes <- c("mpg", "disp")
exposures <- c("cyl")

result <- run_linear_models(
  data = mtcars,
  outcome = outcomes,
  exposure = exposures
)
#> Warning in formula.character(object, env = baseenv()): Using formula(x) is deprecated when x is a character vector of length > 1.
#>   Consider formula(paste(x, collapse = " ")) instead.

result$tidy
#> # A tibble: 2 × 9
#>   term        estimate conf.low conf.high std.error  p.value error n_obs   BIC
#>   <chr>          <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <lgl> <int> <dbl>
#> 1 (Intercept)    37.9     33.6      42.1      2.07  8.37e-18 NA       32  174.
#> 2 cyl            -2.88    -3.53     -2.22     0.322 6.11e-10 NA       32  174.
```

You can check the help section of the run_linear_models() (by using
“?run_linear_models() in R) or the run_linear_models guide under”Learn
More” for more examples.

## Notes

Deprecation warning: You may see a warning related to
formula.character() when running examples. This originates from a
dependency package and does not affect the functionality or results of
turtle. The issue is expected to be resolved in a future update of the
underlying package.

## Features

- Fits linear (`lm`) or mixed effects (`lmer`) models
- Supports:
  - Covariates
  - Effect modifiers
  - Sensitivity covariates
  - Random effects
- Returns a named list of model results, each containing:
  - `model`: The fitted model object (`lm` or `lmer`)
  - `tidy`: A tidy summary of model coefficients
  - `residuals`: Model residuals
  - `formula`: The model formula used
  - `exposure`: The exposure variable used
  - `model_name`: A unique identifier for the model
- If `return_grid = TRUE`, the full model grid is attached as an
  attribute

## Learn More

## License

MIT © drhealy013
