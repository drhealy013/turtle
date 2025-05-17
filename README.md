
# turtle

<!-- badges: start -->

[![R-CMD-check](https://github.com/drhealy013/turtle)](https://github.com/drhealy013/turtle/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/drhealy013/turtle/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/drhealy013/turtle/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/drhealy013/turtle/graph/badge.svg)](https://app.codecov.io/gh/drhealy013/turtle)
<!-- badges: end -->

`turtle` is an R package that provides simplified, user-friendly
functions for common data analysis tasks such as modeling,
dimensionality reduction, and visualization. It is designed to support
users with minimal programming experience by allowing them to specify
key inputs (e.g., outcome, exposure, covariates) and automatically
handle the underlying code. It is applicable to a wide range of data
types, including epidemiological, ecological, and experimental datasets.

## Installation

You can install the development version of `turtle` from GitHub with:

``` r
# install.packages("pak")
pak::pak("drhealy013/turtle")
```

## Example

Here’s a basic example using the built-in `mtcars` dataset:

``` r
library(turtle)

result <- run_linear_models(
  data = mtcars,
  outcome = "mpg",
  exposure = "wt"
)

result$tidy
#> # A tibble: 2 × 9
#>   term        estimate conf.low conf.high std.error  p.value error n_obs   BIC
#>   <chr>          <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <lgl> <int> <dbl>
#> 1 (Intercept)    37.3     33.5      41.1      1.88  8.24e-19 NA       32  170.
#> 2 wt             -5.34    -6.49     -4.20     0.559 1.29e-10 NA       32  170.
```

## Features

- Fits linear (`lm`) or mixed effects (`lmer`) models
- Supports:
  - Covariates
  - Effect modifiers
  - Sensitivity covariates
  - Random effects
- Returns:
  - Tidy model summary
  - Residuals
  - Model formula

## License

MIT © drhealy013
