[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) 
[![Travis-CI Build Status](https://travis-ci.org/larmarange/labelled.svg?branch=master)](https://travis-ci.org/larmarange/labelled)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/labelled)](http://cran.r-project.org/web/packages/labelled) 
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/labelled)

# Labelled package for R

This package is built on the new class `labelled` introduced by **haven** package
and propose additional functions to manipulate labelled data.

Please note that this package is still under active development.

## Introduction to labelled

Read https://github.com/larmarange/labelled/blob/master/vignettes/intro_labelled.Rmd

## Some general principles

1. Functions are intented to support `labelled` metadata structures only.
   However, `to_labelled` method allows to convert metadata 
   from **foreign** and **memisc** packages.
2. Functions should, by default, modify metadata only (i.e. classes and attributes), except if
   explicitly expressed by the user.

