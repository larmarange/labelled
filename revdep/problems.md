# jsmodule

<details>

* Version: 1.0.7
* Source code: https://github.com/cran/jsmodule
* URL: https://github.com/jinseob2kim/jsmodule
* BugReports: https://github.com/jinseob2kim/jsmodule/issues
* Date/Publication: 2020-04-29 17:20:09 UTC
* Number of recursive dependencies: 189

Run `revdep_details(,"jsmodule")` for more info

</details>

## Newly broken

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      'data.table', 'shiny', 'readxl', 'DT', 'jstable', 'epiDisplay',
      'GGally', 'ggplot2', 'rstudioapi', 'shinycustomloader', 'MatchIt',
      'survey', 'tableone', 'jskm', 'geepack', 'maxstat', 'survC1',
      'survIDINRI', 'timeROC', 'devEMF', 'shinyWidgets', 'pROC', 'Hmisc',
      'see', 'RColorBrewer', 'Cairo'
    
    Packages suggested but not available for checking: 'testthat', 'knitr'
    
    VignetteBuilder package required for checking but not installed: 'knitr'
    
    See section 'The DESCRIPTION file' in the 'Writing R Extensions'
    manual.
    ```

## Newly fixed

*   checking examples ... ERROR
    ```
    ...
    You are welcome to redistribute it under certain conditions.
    Type 'license()' or 'licence()' for distribution details.
    
      Natural language support but running in an English locale
    
    R is a collaborative project with many contributors.
    Type 'contributors()' for more information and
    'citation()' on how to cite R or R packages in publications.
    
    Type 'demo()' for some demos, 'help()' for on-line help, or
    'help.start()' for an HTML browser interface to help.
    Type 'q()' to quit R.
    
    > pkgname <- "jsmodule"
    > source(file.path(R.home("share"), "R", "examples-header.R"))
    > options(warn = 1)
    > options(pager = "console")
    > library('jsmodule')
    Error: package or namespace load failed for 'jsmodule' in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     there is no package called 'ggplot2'
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Complete output:
      > library(testthat)
      Error in library(testthat) : there is no package called 'testthat'
      Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      'jsmodule.Rmd' using 'UTF-8'... failed
     ERROR
    Errors in running code in vignettes:
    when running code in 'jsmodule.Rmd'
      ...
    
      When running vignette 'jsmodule.Rmd':
    Erreur : No such vignette 'jsmodule.Rmd'
    Exécution arrêtée
    ```

*   checking for missing documentation entries ... WARNING
    ```
    ...
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps, exclude, include.only)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc, character.only
    Exécution arrêtée
    All user-level objects in a package should have documentation entries.
    See chapter 'Writing R documentation files' in the 'Writing R
    Extensions' manual.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
    Séquence d'appel :
    6: stop(msg, call. = FALSE, domain = NA)
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps, exclude, include.only)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc, character.only = TRUE, ve
    Exécution arrêtée
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      'Cairo' 'survC1'
      All declared Imports should be used.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    ...
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps, exclude, include.only)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc, character.only = TRUE, ve
    Exécution arrêtée
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter 'Writing R documentation files' in the 'Writing R
    Extensions' manual.
    ```

# questionr

<details>

* Version: 0.7.0
* Source code: https://github.com/cran/questionr
* URL: https://juba.github.io/questionr/
* BugReports: https://github.com/juba/questionr/issues
* Date/Publication: 2018-11-26 13:10:06 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"questionr")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Package non disponible pour vérifier les xrefs Rd : 'Hmisc'
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4145 marked UTF-8 strings
    ```

## Newly fixed

*   checking package dependencies ... ERROR
    ```
    Package required but not available: 'labelled'
    
    See section 'The DESCRIPTION file' in the 'Writing R Extensions'
    manual.
    ```

# RSDA

<details>

* Version: 3.0.3
* Source code: https://github.com/cran/RSDA
* URL: http://www.oldemarrodriguez.com
* Date/Publication: 2020-04-16 22:10:02 UTC
* Number of recursive dependencies: 127

Run `revdep_details(,"RSDA")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      'labelled' 'randomcoloR'
      All declared Imports should be used.
    ```

## Newly fixed

*   checking package dependencies ... ERROR
    ```
    Package required but not available: 'labelled'
    
    See section 'The DESCRIPTION file' in the 'Writing R Extensions'
    manual.
    ```

