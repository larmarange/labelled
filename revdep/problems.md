# questionr

<details>

* Version: 0.7.1
* Source code: https://github.com/cran/questionr
* URL: https://juba.github.io/questionr/
* BugReports: https://github.com/juba/questionr/issues
* Date/Publication: 2020-05-26 11:30:05 UTC
* Number of recursive dependencies: 81

Run `revdep_details(,"questionr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: 'vctrs'
    Namespace in Imports field not imported from: 'haven'
      All declared Imports should be used.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'lookfor':
    lookfor
      Code: function(data, ..., labels = TRUE, ignore.case = TRUE, details
                     = TRUE)
      Docs: function(data, ..., labels = TRUE, ignore.case = TRUE, details
                     = FALSE)
      Mismatches in argument default values:
        Name: 'details' Code: TRUE Docs: FALSE
    look_for
      Code: function(data, ..., labels = TRUE, ignore.case = TRUE, details
                     = TRUE)
      Docs: function(data, ..., labels = TRUE, ignore.case = TRUE, details
                     = FALSE)
      Mismatches in argument default values:
        Name: 'details' Code: TRUE Docs: FALSE
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: 'haven'
      All declared Imports should be used.
    ```

## In both

*   checking Rd cross-references ... WARNING
    ```
    Package non disponible pour vérifier les xrefs Rd : 'Hmisc'
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4145 marked UTF-8 strings
    ```

