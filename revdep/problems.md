# codebook

Version: 0.8.1

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘skimr’ ‘userfriendlyscience’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# crosswalkr

Version: 0.2.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      Error: package or namespace load failed for 'testthat' in rbind(info, getNamespaceInfo(env, "S3methods")):
       number of columns of matrices must match (see arg 2)
      In addition: Warning message:
      package 'testthat' was built under R version 3.6.0 
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# jsmodule

Version: 0.8.3

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available:
      ‘jstable’ ‘jskm’ ‘survIDINRI’ ‘timeROC’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# jstable

Version: 0.8.1

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

*   checking examples ... ERROR
    ```
    Running examples in ‘questionr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: odds.ratio
    > ### Title: Odds Ratio
    > ### Aliases: odds.ratio odds.ratio.glm odds.ratio.multinom
    > ###   odds.ratio.factor odds.ratio.table odds.ratio.matrix
    > ###   odds.ratio.numeric print.odds.ratio
    > 
    > ### ** Examples
    > 
    > data(hdv2003)
    > reg <- glm(cinema ~ sexe + age, data=hdv2003, family=binomial)
    > odds.ratio(reg)
    Error in confint.glm(x, level = level) : package 'MASS' must be installed
    Calls: odds.ratio ... odds.ratio.glm -> cbind -> <Anonymous> -> confint.glm
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      Error: package or namespace load failed for 'testthat' in rbind(info, getNamespaceInfo(env, "S3methods")):
       number of columns of matrices must match (see arg 2)
      In addition: Warning message:
      package 'testthat' was built under R version 3.6.0 
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘memisc’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4145 marked UTF-8 strings
    ```

# SEERaBomb

Version: 2019.1

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

*   checking examples ... ERROR
    ```
    ...
                                                           
     spiders 418  106   25.4     0  222    71.2        71.2
                                 1   90    28.8       100.0
                                                           
       edema 418    0    0.0     0  354    84.7        84.7
                               0.5   44    10.5        95.2
                                 1   20     4.8       100.0
                                                           
       stage 418    6    1.4     1   21     5.1         5.1
                                 2   92    22.3        27.4
                                 3  155    37.6        65.0
                                 4  144    35.0       100.0
                                                           
    > 
    > ## The table can be stratified by one or more variables
    > catTableBySexTrt <- CreateCatTable(vars = catVars,
    +                                    strata = c("sex","trt"), data = pbc)
    Error in rbind(info, getNamespaceInfo(env, "S3methods")) : 
      number of columns of matrices must match (see arg 2)
    Calls: CreateCatTable ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      > ## Reference: http://adv-r.had.co.nz/Testing.html
      > ## See the testthat subdirectory for actual test code
      > ## Created on: 2014-06-01
      > ## Author: Kazuki Yoshida
      > ################################################################################
      > 
      > 
      > ### Run all tests
      > ################################################################################
      > library(testthat)
      Error: package or namespace load failed for 'testthat' in rbind(info, getNamespaceInfo(env, "S3methods")):
       number of columns of matrices must match (see arg 2)
      In addition: Warning message:
      package 'testthat' was built under R version 3.6.0 
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Matching’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# tidyqwi

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      Error: package or namespace load failed for 'testthat' in rbind(info, getNamespaceInfo(env, "S3methods")):
       number of columns of matrices must match (see arg 2)
      In addition: Warning message:
      package 'testthat' was built under R version 3.6.0 
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plyr’
      All declared Imports should be used.
    ```

