
<!-- README.md is generated from README.Rmd. Please edit that file -->

# labelled <img src="man/figures/labelled.png" align="right" width="120" />

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build
Status](https://travis-ci.org/larmarange/labelled.svg?branch=master)](https://travis-ci.org/larmarange/labelled)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/labelled)](https://cran.r-project.org/package=labelled)
[![Rdoc](http://www.rdocumentation.org/badges/version/labelled)](http://www.rdocumentation.org/packages/labelled)
[![Downloads](https://cranlogs.r-pkg.org/badges/labelled)](https://cran.r-project.org/package=labelled)
[![DOI](https://www.zenodo.org/badge/38772078.svg)](https://www.zenodo.org/badge/latestdoi/38772078)

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

Latest version from GitHub:

``` r
devtools::install_github("larmarange/labelled")
```

## Introduction to labelled

Read the vignette at
<http://larmarange.github.io/labelled/articles/intro_labelled.html>

## Breaking changes introduced in version 2.0.0

Following version 2.0.0 of `haven`, `labelled()` and `labelled_spss()`
now produce objects with class “haven\_labelled” and
“haven\_labelled\_spss”, due to conflict between the previous
“labelled” class and the “labelled” class used by `Hmisc`.

A new function `update_labelled()` could be used to convert data
imported with an older version of `haven` to the new classes.

## Some general principles

1.  Functions are intented to support `labelled` metadata structures
    only. However, `to_labelled` method allows to convert metadata from
    **foreign** and **memisc** packages.
2.  Functions should, by default, modify metadata only (i.e. classes and
    attributes), except if explicitly expressed by the user.
