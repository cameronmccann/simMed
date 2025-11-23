
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simMed

<!-- badges: start -->

<!-- badges: end -->

The goal of `simMed` is to provide tools for generating clustered
(multilevel) datasets for causal mediation analysis. The package
currently includes a single function, `generate_data()`, that simulates
clustered data with individuals nested within clusters. Each simulated
dataset includes an individual-level treatment (`A`), mediator (`M`),
outcome (`Y`), pretreatment covariates (`X`), and a cluster-level
confounder (`Z`). The function supports both binary and continuous
mediator/outcome and provides the true potential outcomes.

## Installation

To install the latest development version from
[GitHub](https://github.com/), type:

``` r
# install.packages("devtools")
devtools::install_github("cameronmccann/simMed")
```

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
