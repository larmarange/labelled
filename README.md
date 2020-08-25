
<!-- README.md is generated from README.Rmd. Please edit that file -->

# labelled <img src="man/figures/labelled.png" align="right" width="120" />

<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/0.1.0/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Travis-CI Build
Status](https://travis-ci.org/larmarange/labelled.svg?branch=master)](https://travis-ci.org/larmarange/labelled)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/labelled)](https://cran.r-project.org/package=labelled)
[![Rdoc](https://www.rdocumentation.org/badges/version/labelled)](https://www.rdocumentation.org/packages/labelled)
[![Downloads](https://cranlogs.r-pkg.org/badges/labelled)](https://cran.r-project.org/package=labelled)
[![DOI](https://www.zenodo.org/badge/38772078.svg)](https://www.zenodo.org/badge/latestdoi/38772078)
<!-- badges: end -->

This package is built on the new classes `haven_labelled` and
`haven_labelled_spss` introduced by `haven` package to handle labelled
variables imported from SPSS, Stata and SAS. The `labelled` package
propose several functions to manipulate such vectors and their metadata:
variable labels, value labels and user-defined missing values.

## Installation

From CRAN:

``` r
install.packages("labelled")
```

Development version from GitHub:

``` r
devtools::install_github("larmarange/labelled")
```

## Introduction

Read the vignette at
<https://larmarange.github.io/labelled/articles/intro_labelled.html>

## Cheatsheet

[![labelled
cheatsheet](https://github.com/larmarange/labelled/raw/master/cheatsheet/labelled_cheatsheet.png)](https://github.com/larmarange/labelled/raw/master/cheatsheet/labelled_cheatsheet.pdf)

## Some general guidelines

1.  Functions are intended to support `labelled` metadata structures
    only. However, `to_labelled()` method allows to convert metadata
    from **foreign** and **memisc** packages.
2.  Functions should, by default, modify metadata only (i.e. classes and
    attributes), except if explicitly expressed by the user.
