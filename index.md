# labelled

This package is built on the new classes `haven_labelled` and
`haven_labelled_spss` introduced by `haven` package to handle labelled
variables imported from SPSS, Stata and SAS. The `labelled` package
propose several functions to manipulate such vectors and their metadata:
variable labels, value labels and user-defined missing values.

## Installation & Documentation

To install **stable version**:

    install.packages("labelled")

Documentation of stable version:
<https://larmarange.github.io/labelled/>

To install **development version**:

    remotes::install_github("larmarange/labelled")

Documentation of development version:
<https://larmarange.github.io/labelled/dev/>

## Introduction

Read the vignette at
<https://larmarange.github.io/labelled/articles/intro_labelled.html>

## Cheatsheet

[![labelled
cheatsheet](https://github.com/larmarange/labelled/raw/main/cheatsheet/labelled_cheatsheet.png)](https://github.com/larmarange/labelled/raw/main/cheatsheet/labelled_cheatsheet.pdf)

## Some general guidelines

1.  Functions are intended to support `labelled` metadata structures
    only. However,
    [`to_labelled()`](https://larmarange.github.io/labelled/reference/to_labelled.md)
    method allows to convert metadata from **foreign** and **memisc**
    packages.
2.  Functions should, by default, modify metadata only (i.e. classes and
    attributes), except if explicitly expressed by the user.
