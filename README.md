
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gest

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/alexhallam/gest.svg?branch=master)](https://travis-ci.org/alexhallam/gest)
<!-- badges: end -->

The goal of gest is to create shrinkage estimates in a flexible and fast
way. This package is a convenient wrapper for the `deconvolveR` package.
\#\# Installation

You can install the released version of gest from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gest")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alexhallam/gest")
```

## Example

In the example below we use the `add_binom_gest` to calculate the
“g-estimates” which are shrinkage estimators. Full posterior
distributions are also calculated along with confidence intervals.

``` r
library(tibble)
library(gest)
set.seed(2017)

# simulate 200 random examples from a beta-binomial
obs <- 200
dat <- tibble(prob = rbeta(obs, 10, 50),
                 n = round(rlnorm(obs, 4, 2)) + 1,
                 x = rbinom(obs, n, prob))
result <- add_binom_gest(dat, x, n)
result
#> # A tibble: 200 x 8
#>      prob     n     x .gest_dist         .raw .gest   .lo   .hi
#>     <dbl> <dbl> <int> <list>            <dbl> <dbl> <dbl> <dbl>
#>  1 0.271      2     1 <tibble [99 x 2]> 0.5    0.18  0.14  0.22
#>  2 0.162      1     0 <tibble [99 x 2]> 0      0.17  0.13  0.21
#>  3 0.213     77    11 <tibble [99 x 2]> 0.143  0.16  0.13  0.18
#>  4 0.0829    57     6 <tibble [99 x 2]> 0.105  0.14  0.11  0.17
#>  5 0.163    117    23 <tibble [99 x 2]> 0.197  0.18  0.16  0.21
#>  6 0.193    934   191 <tibble [99 x 2]> 0.204  0.2   0.19  0.21
#>  7 0.0746     6     0 <tibble [99 x 2]> 0      0.16  0.12  0.19
#>  8 0.167     37     6 <tibble [99 x 2]> 0.162  0.17  0.14  0.2 
#>  9 0.153     85    11 <tibble [99 x 2]> 0.129  0.15  0.12  0.17
#> 10 0.187   1205   226 <tibble [99 x 2]> 0.188  0.19  0.18  0.2 
#> # ... with 190 more rows
```

# Related Work

[ebbr](https://github.com/dgrtwo/ebbr)

# Academic Work

[deconvolveR](https://cran.r-project.org/web/packages/deconvolveR/deconvolveR.pdf)
