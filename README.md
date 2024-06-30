# hidradenitis

<!-- badges: start -->
[![R-CMD-check](https://github.com/billdenney/hidradenitis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billdenney/hidradenitis/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/billdenney/hidradenitis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/billdenney/hidradenitis?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/hidradenitis)](https://CRAN.R-project.org/package=hidradenitis)
<!-- badges: end -->

The goal of the `hidradenitis` package is to support use and interpretation of
clinical scores for hidradenitis suppurativa.

## Installation

### Stable version

You can install the stable version of hidradenitis from CRAN with:

``` r
install.packages("devtools")
```

### Development version

You can install the development version of hidradenitis from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("billdenney/hidradenitis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hidradenitis)

hasi_r_num(
  bsa_percent_within_site = c(0, 0, 0, 0, 5, 1, 4.3, 1.2, 6.8, 7.2),
  bodysite =
    c("Right Axilla", "Buttocks including Intergluteal Cleft",
      "Back", "Left Thigh", "Head & Neck", "Left Axilla",
      "Chest", "Pubis & Genitals", "Abdomen", "Right Thigh"),
  inflam_color_chg = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0),
  induration = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0),
  open_skin_surface = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0),
  tunnels = c(0, 0, 0, 0, 2, 3, 1, 3, 2, 0)
)

hiscr(
  baseline_abscess = c(3, 2, 4),
  baseline_nodule = c(5, 4, 6),
  baseline_fistula = c(2, 1, 3),
  timepoint_abscess = c(1, 1, 2),
  timepoint_nodule = c(2, 3, 2),
  timepoint_fistula = c(2, 1, 3),
  percentage = 50
)

hs_pga <-
  hs_pga_num(
    abscess_fistula = c(0, 0, 1, 0, 1, 2, 6),
    inflammatory_nodule = c(0, 0, 0, 3, 5, 8, 12),
    non_inflammatory_nodule = c(0, 1, 0, 0, 0, 0, 0)
  )
hs_pga_char(hs_pga)

ihs4 <-
  ihs4_num(
    nodules = c(5, 3, 2),
    abscesses = c(2, 1, 0),
    draining_tunnels = c(1, 2, 3)
  )
ihs4_char(ihs4)

```
