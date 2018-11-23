# codebook

Version: 0.6.3

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘codebook-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: codebook_component_scale
    > ### Title: Codebook component for scales
    > ### Aliases: codebook_component_scale
    > 
    > ### ** Examples
    > 
    > # will generate figures in a temporary directory
    > old_base_dir <- knitr::opts_knit$get("base.dir")
    > knitr::opts_knit$set(base.dir = tempdir())
    > on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
    > data("bfi")
    > bfi <- bfi[,c("BFIK_open", paste0("BFIK_open_", 1:4))]
    > codebook_component_scale(bfi[,1], "BFIK_open", bfi[,-1],
    +    reliabilities = list(BFIK_open = psych::alpha(bfi[,-1])))
    Error: C stack usage  7970144 is too close to the limit
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Erreur : C stack usage  7970176 is too close to the limit
    Exécution arrêtée
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘graphics’ ‘pander’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 65 marked UTF-8 strings
    ```

# crosswalkr

Version: 0.1.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(crosswalkr)
      > 
      > test_check("crosswalkr")
      [31m──[39m [31m1. Failure: Not proper labelled class (@test-encodefrom.R#40) [39m [31m─────────────────────────────────────────────────────────────[39m
      `vec` inherits from `haven_labelled` not `labelled`.
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════
      OK: 36 SKIPPED: 0 FAILED: 1
      1. Failure: Not proper labelled class (@test-encodefrom.R#40) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# questionr

Version: 0.6.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4145 marked UTF-8 strings
    ```

# tableone

Version: 0.9.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

