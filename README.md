# Labelled package for R

This package is built on the new class `labelled` introduced by **haven** package
and propose additional functions to manipulate labelled data.

Please note that this package is still under active development.

## Introduction to labelled

Read https://github.com/larmarange/labelled/blob/master/vignettes/intro_labelled.Rmd

## Some general principles

1. Functions are intented to support `labelled` metadata structures only.
   However, `foreign_to_labelled` and `memisc_to_labelles` allows to convert metadata 
   from **foreign** and **memisc** packages.
2. Functions should, by default, modify metadata only (i.e. classes and attributes), except if
   explicitly expressed by the user.

