pkgname <- "tableone"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('tableone')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CreateCatTable")
### * CreateCatTable

flush(stderr()); flush(stdout())

### Name: CreateCatTable
### Title: Create an object summarizing categorical variables
### Aliases: CreateCatTable

### ** Examples


## Load
library(tableone)

## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)

## Create an overall table for categorical variables
catVars <- c("status","ascites","hepato","spiders","edema","stage")
catTableOverall <- CreateCatTable(vars = catVars, data = pbc)

## Simply typing the object name will invoke the print.CatTable method,
## which will show the sample size, frequencies and percentages.
## For 2-level variables, only the higher level is shown for simplicity
## unless the variables are specified in the cramVars argument.
catTableOverall

## If you need to show both levels for some 2-level factors, use cramVars
print(catTableOverall, cramVars = "hepato")

## Use the showAllLevels argument to see all levels for all variables.
print(catTableOverall, showAllLevels = TRUE)

## You can choose form frequencies ("f") and/or percentages ("p") or both.
## "fp" frequency (percentage) is the default. Row names change accordingly.
print(catTableOverall, format = "f")
print(catTableOverall, format = "p")

## To further examine the variables, use the summary.CatTable method,
## which will show more details.
summary(catTableOverall)

## The table can be stratified by one or more variables
catTableBySexTrt <- CreateCatTable(vars = catVars,
                                   strata = c("sex","trt"), data = pbc)

## print now includes p-values which are by default calculated by chisq.test.
## It is formatted at the decimal place specified by the pDigits argument
## (3 by default). It is formatted like <0.001 if very small.
catTableBySexTrt

## The exact argument toggles the p-values to the exact test result from
## fisher.test. It will show which ones are from exact tests.
print(catTableBySexTrt, exact = "ascites")

## summary now includes both types of p-values
summary(catTableBySexTrt)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(catTableBySexTrt, exact = "ascites", quote = TRUE)

## If you want to center-align values in Word, use noSpaces option.
print(catTableBySexTrt, exact = "ascites", quote = TRUE, noSpaces = TRUE)




cleanEx()
nameEx("CreateContTable")
### * CreateContTable

flush(stderr()); flush(stdout())

### Name: CreateContTable
### Title: Create an object summarizing continous variables
### Aliases: CreateContTable

### ** Examples


## Load
library(tableone)

## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)

## Create an overall table for continuous variables
contVars <- c("time","age","bili","chol","albumin","copper",
              "alk.phos","ast","trig","platelet","protime")
contTableOverall <- CreateContTable(vars = contVars, data = pbc)

## Simply typing the object name will invoke the print.ContTable method,
## which will show the sample size, means and standard deviations.
contTableOverall

## To further examine the variables, use the summary.ContTable method,
## which will show more details.
summary(contTableOverall)

## c("age","chol","copper","alk.phos","trig","protime") appear highly skewed.
## Specify them in the nonnormal argument, and the display changes to the median,
## and the [25th, 75th] percentile.
nonNormalVars <- c("age","chol","copper","alk.phos","trig","protime")
print(contTableOverall, nonnormal = nonNormalVars)

## To show median [min,max] for nonnormal variables, use minMax = TRUE
print(contTableOverall, nonnormal = nonNormalVars, minMax = TRUE)

## The table can be stratified by one or more variables
contTableBySexTrt <- CreateContTable(vars = contVars,
                                     strata = c("sex","trt"), data = pbc)

## print now includes p-values which are by default calculated by oneway.test (t-test
## equivalent in the two group case). It is formatted at the decimal place specified
## by the pDigits argument (3 by default). It does <0.001 for you.
contTableBySexTrt

## The nonnormal argument toggles the p-values to the nonparametric result from
## kruskal.test (wilcox.test equivalent for the two group case).
print(contTableBySexTrt, nonnormal = nonNormalVars)

## summary now includes both types of p-values
summary(contTableBySexTrt)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE)

## If you want to center-align values in Word, use noSpaces option.
print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE, noSpaces = TRUE)




cleanEx()
nameEx("CreateTableOne")
### * CreateTableOne

flush(stderr()); flush(stdout())

### Name: CreateTableOne
### Title: Create an object summarizing both continuous and categorical
###   variables
### Aliases: CreateTableOne

### ** Examples


## Load
library(tableone)

## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)

## Make categorical variables factors
varsToFactor <- c("status","trt","ascites","hepato","spiders","edema","stage")
pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)

## Create a variable list
dput(names(pbc))
vars <- c("time","status","age","sex","ascites","hepato",
          "spiders","edema","bili","chol","albumin",
          "copper","alk.phos","ast","trig","platelet",
          "protime","stage")

## Create Table 1 stratified by trt
tableOne <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc)

## Just typing the object name will invoke the print.TableOne method
tableOne

## Specifying nonnormal variables will show the variables appropriately,
## and show nonparametric test p-values. Specify variables in the exact
## argument to obtain the exact test p-values. cramVars can be used to
## show both levels for a 2-level categorical variables.
print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
      exact = c("status","stage"), cramVars = "hepato", smd = TRUE)

## Use the summary.TableOne method for detailed summary
summary(tableOne)

## See the categorical part only using $ operator
tableOne$CatTable
summary(tableOne$CatTable)

## See the continuous part only using $ operator
tableOne$ContTable
summary(tableOne$ContTable)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
      exact = c("status","stage"), quote = TRUE)

## If you want to center-align values in Word, use noSpaces option.
print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
      exact = c("status","stage"), quote = TRUE, noSpaces = TRUE)

## If SMDs are needed as numericals, use ExtractSmd()
ExtractSmd(tableOne)




cleanEx()
nameEx("ExtractSmd")
### * ExtractSmd

flush(stderr()); flush(stdout())

### Name: ExtractSmd
### Title: Extract standardized mean differences from a (svy)TableOne
###   object
### Aliases: ExtractSmd

### ** Examples


## See examples for CreateTableOne and svyCreateTableOne




cleanEx()
nameEx("ShowRegTable")
### * ShowRegTable

flush(stderr()); flush(stdout())

### Name: ShowRegTable
### Title: Format regression results in medically decent format
### Aliases: ShowRegTable

### ** Examples


## Load
library(tableone)

## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)

## Fit a Cox regression model
objCoxph <- coxph(formula = Surv(time, status == 2) ~ trt + age + albumin + ascites,
                  data    = pbc)

## Show the simple table
ShowRegTable(objCoxph)

## Show with quote to ease copy and paste
ShowRegTable(objCoxph, quote = TRUE)




cleanEx()
nameEx("print.CatTable")
### * print.CatTable

flush(stderr()); flush(stdout())

### Name: print.CatTable
### Title: Format and print 'CatTable' class objects
### Aliases: print.CatTable

### ** Examples


## Load
library(tableone)

## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)

## Create an overall table for categorical variables
catVars <- c("status","ascites","hepato","spiders","edema","stage")
catTableOverall <- CreateCatTable(vars = catVars, data = pbc)

## Simply typing the object name will invoke the print.CatTable method,
## which will show the sample size, frequencies and percentages.
## For 2-level variables, only the higher level is shown for simplicity.
catTableOverall

## If you need to show both levels for some 2-level factors, use cramVars
print(catTableOverall, cramVars = "hepato")

## Use the showAllLevels argument to see all levels for all variables.
print(catTableOverall, showAllLevels = TRUE)

## You can choose form frequencies ("f") and/or percentages ("p") or both.
## "fp" frequency (percentage) is the default. Row names change accordingly.
print(catTableOverall, format = "f")
print(catTableOverall, format = "p")

## To further examine the variables, use the summary.CatTable method,
## which will show more details.
summary(catTableOverall)

## The table can be stratified by one or more variables
catTableBySexTrt <- CreateCatTable(vars = catVars,
                                   strata = c("sex","trt"), data = pbc)

## print now includes p-values which are by default calculated by chisq.test.
## It is formatted at the decimal place specified by the pDigits argument
## (3 by default). It does <0.001 for you.
catTableBySexTrt

## The exact argument toggles the p-values to the exact test result from
## fisher.test. It will show which ones are from exact tests.
print(catTableBySexTrt, exact = "ascites")

## summary now includes both types of p-values
summary(catTableBySexTrt)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(catTableBySexTrt, exact = "ascites", quote = TRUE)

## If you want to center-align values in Word, use noSpaces option.
print(catTableBySexTrt, exact = "ascites", quote = TRUE, noSpaces = TRUE)




cleanEx()
nameEx("print.ContTable")
### * print.ContTable

flush(stderr()); flush(stdout())

### Name: print.ContTable
### Title: Format and print 'ContTable' class objects
### Aliases: print.ContTable

### ** Examples


## Load
library(tableone)

## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)

## Create an overall table for continuous variables
contVars <- c("time","age","bili","chol","albumin","copper",
              "alk.phos","ast","trig","platelet","protime")
contTableOverall <- CreateContTable(vars = contVars, data = pbc)

## Simply typing the object name will invoke the print.ContTable method,
## which will show the sample size, means and standard deviations.
contTableOverall

## To further examine the variables, use the summary.ContTable method,
## which will show more details.
summary(contTableOverall)

## c("age","chol","copper","alk.phos","trig","protime") appear highly skewed.
## Specify them in the nonnormal argument, and the display changes to the median,
## and the [25th, 75th] percentile.
nonNormalVars <- c("age","chol","copper","alk.phos","trig","protime")
print(contTableOverall, nonnormal = nonNormalVars)

## To show median [min,max] for nonnormal variables, use minMax = TRUE
print(contTableOverall, nonnormal = nonNormalVars, minMax = TRUE)

## The table can be stratified by one or more variables
contTableBySexTrt <- CreateContTable(vars = contVars,
                                     strata = c("sex","trt"), data = pbc)

## print now includes p-values which are by default calculated by oneway.test (t-test
## equivalent in the two group case). It is formatted at the decimal place specified
## by the pDigits argument (3 by default). It does <0.001 for you.
contTableBySexTrt

## The nonnormal argument toggles the p-values to the nonparametric result from
## kruskal.test (wilcox.test equivalent for the two group case).
print(contTableBySexTrt, nonnormal = nonNormalVars)

## The minMax argument toggles whether to show median [range]
print(contTableBySexTrt, nonnormal = nonNormalVars, minMax = TRUE)

## summary now includes both types of p-values
summary(contTableBySexTrt)

## If your work flow includes copying to Excel and Word when writing manuscripts,
## you may benefit from the quote argument. This will quote everything so that
## Excel does not mess up the cells.
print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE)

## If you want to center-align values in Word, use noSpaces option.
print(contTableBySexTrt, nonnormal = nonNormalVars, quote = TRUE, noSpaces = TRUE)




cleanEx()
nameEx("print.TableOne")
### * print.TableOne

flush(stderr()); flush(stdout())

### Name: print.TableOne
### Title: Format and print 'TableOne' class objects
### Aliases: print.TableOne

### ** Examples


## See examples for CreateTableOne and svyCreateTableOne




cleanEx()
nameEx("print.svyCatTable")
### * print.svyCatTable

flush(stderr()); flush(stdout())

### Name: print.svyCatTable
### Title: Format and print 'svyCatTable' class objects
### Aliases: print.svyCatTable

### ** Examples


## See the examples for svyCreateTableOne()




cleanEx()
nameEx("print.svyContTable")
### * print.svyContTable

flush(stderr()); flush(stdout())

### Name: print.svyContTable
### Title: Format and print 'svyContTable' class objects
### Aliases: print.svyContTable

### ** Examples


## See the examples for svyCreateTableOne()




cleanEx()
nameEx("summary.CatTable")
### * summary.CatTable

flush(stderr()); flush(stdout())

### Name: summary.CatTable
### Title: Shows all results in a 'CatTable' class object
### Aliases: summary.CatTable

### ** Examples


## See examples for CreateTableOne




cleanEx()
nameEx("summary.ContTable")
### * summary.ContTable

flush(stderr()); flush(stdout())

### Name: summary.ContTable
### Title: Shows all results in a 'ContTable' class object
### Aliases: summary.ContTable

### ** Examples


## See examples for CreateTableOne




cleanEx()
nameEx("summary.TableOne")
### * summary.TableOne

flush(stderr()); flush(stdout())

### Name: summary.TableOne
### Title: Shows all results in a '(svy)TableOne' class object
### Aliases: summary.TableOne

### ** Examples


## See examples for CreateTableOne and svyCreateTableOne




cleanEx()
nameEx("summary.svyCatTable")
### * summary.svyCatTable

flush(stderr()); flush(stdout())

### Name: summary.svyCatTable
### Title: Shows all results in a 'svyCatTable' class object
### Aliases: summary.svyCatTable

### ** Examples


## See the examples for svyCreateTableOne()




cleanEx()
nameEx("summary.svyContTable")
### * summary.svyContTable

flush(stderr()); flush(stdout())

### Name: summary.svyContTable
### Title: Shows all results in a 'svyContTable' class object
### Aliases: summary.svyContTable

### ** Examples


## See the examples for svyCreateTableOne()




cleanEx()
nameEx("svyCreateCatTable")
### * svyCreateCatTable

flush(stderr()); flush(stdout())

### Name: svyCreateCatTable
### Title: Create an object summarizing categorical variables for weighted
###   data
### Aliases: svyCreateCatTable

### ** Examples


## See the examples for svyCreateTableOne()




cleanEx()
nameEx("svyCreateContTable")
### * svyCreateContTable

flush(stderr()); flush(stdout())

### Name: svyCreateContTable
### Title: Create an object summarizing continous variables for weighted
###   dataset
### Aliases: svyCreateContTable

### ** Examples


## See the examples for svyCreateTableOne()




cleanEx()
nameEx("svyCreateTableOne")
### * svyCreateTableOne

flush(stderr()); flush(stdout())

### Name: svyCreateTableOne
### Title: Create an object summarizing both continuous and categorical
###   variables for weighted data
### Aliases: svyCreateTableOne

### ** Examples


## Load packages
library(tableone)
library(survey)

## Create a weighted survey design object
data(nhanes)
nhanesSvy <- svydesign(ids = ~ SDMVPSU, strata = ~ SDMVSTRA, weights = ~ WTMEC2YR,
                       nest = TRUE, data = nhanes)

## Create a table object
## factorVars are converted to factors; no need for variables already factors
## strata will stratify summaries; leave it unspecified for overall summaries
tab1 <- svyCreateTableOne(vars = c("HI_CHOL","race","agecat","RIAGENDR"),
                          strata = "RIAGENDR", data = nhanesSvy,
                          factorVars = c("race","RIAGENDR"))

## Detailed output
summary(tab1)

## Default formatted printing
tab1

## nonnormal specifies variables to be shown as median [IQR]
print(tab1, nonnormal = "HI_CHOL", contDigits = 3, catDigits = 2,
      pDigits = 4, smd = TRUE)

## minMax changes it to median [min, max]
print(tab1, nonnormal = "HI_CHOL", minMax = TRUE, contDigits = 3,
      catDigits = 2, pDigits = 4, smd = TRUE)

## showAllLevels can be used tow show levels for all categorical variables
print(tab1, showAllLevels = TRUE, smd = TRUE)

## To see all printing options
?print.TableOne

## To examine categorical variables only
tab1$CatTable

## To examine continuous variables only
tab1$ContTable

## If SMDs are needed as numericals, use ExtractSmd()
ExtractSmd(tab1)




cleanEx()
nameEx("tableone-package")
### * tableone-package

flush(stderr()); flush(stdout())

### Name: tableone-package
### Title: Create "Table 1" to describe baseline characteristics
### Aliases: tableone-package tableone

### ** Examples


## See examples for CreateTableOne and svyCreateTableOne




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
