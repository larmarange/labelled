# crosswalkr

Version: 0.2.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: renamefrom
    > ### Title: Rename data frame columns using external crosswalk file.
    > ### Aliases: renamefrom renamefrom_
    > 
    > ### ** Examples
    > 
    > df <- data.frame(state = c('Kentucky','Tennessee','Virginia'),
    +                  fips = c(21,47,51),
    +                  region = c('South','South','South'))
    > 
    > cw <- data.frame(old_name = c('state','fips'),
    +                  new_name = c('stname','stfips'),
    +                  label = c('Full state name', 'FIPS code'))
    > 
    > df1 <- renamefrom(df, cw, old_name, new_name, label)
    Error in `var_label<-.data.frame`(`*tmp*`, value = null_label_list) : 
      some variables not found in x
    Calls: renamefrom ... renamefrom_ -> <Anonymous> -> var_label<-.data.frame
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("crosswalkr")
      [31m──[39m [31m1. Error: (unknown) (@test-recodefrom.R#34) [39m [31m──────────────────────────────────────────────────────────────────────[39m
      some variables not found in x
      1: renamefrom(df, crosswalk_file, raw_, clean_, label_) at testthat/test-recodefrom.R:34
      2: renamefrom_(.data, cw_file, raw, clean, label, delimiter, sheet, drop_extra, case_ignore, keep_label, name_label)
      3: labelled::`var_label<-`(`*tmp*`, value = null_label_list)
      4: `var_label<-.data.frame`(`*tmp*`, value = null_label_list)
      5: stop("some variables not found in x")
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 34 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test-recodefrom.R#34) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    
    LABELLED 2.0.0: BREAKING CHANGE
    
    Following version 2.0.0 of `haven`, `labelled()` and `labelled_spss()` now produce objects with class 'haven_labelled' and 'haven_labelled_spss', due to conflict between the previous 'labelled' class and the 'labelled' class used by `Hmisc`.
    
    A new function `update_labelled()` could be used to convert data imported with an older version of `haven`/`labelled` to the new classes.
    
    Quitting from lines 149-155 (crosswalkr.Rmd) 
    Erreur : le traitement de la vignette 'crosswalkr.Rmd' a échoué avec le diagnostic :
    some variables not found in x
    Exécution arrêtée
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# jstable

Version: 0.7.9

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘coxme’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# questionr

Version: 0.7.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      21: stringr::str_length
      22: getExportedValue(pkg, name)
      23: asNamespace(ns)
      24: getNamespace(ns)
      25: tryCatch(loadNamespace(name), error = function(e) stop(e))
      26: tryCatchList(expr, classes, parentenv, handlers)
      27: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      28: value[[3L]](cond)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 44 SKIPPED: 0 FAILED: 1
      1. Error: prop, cprop and lprop tabyl versions are correct (@test_tables.R#88) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Erreur : le traitement de la vignette 'recoding_addins.Rmd' a échoué avec le diagnostic :
    shared object 'stringi.so' not found
    Exécution arrêtée
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4145 marked UTF-8 strings
    ```

# SEERaBomb

Version: 2018.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘demography’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# tableone

Version: 0.10.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

