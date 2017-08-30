<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{labelled}
-->

# Introduction to labelled

The purpose of the **labelled** package is to provide functions to manipulate
metadata as variable labels, value labels and defined missing values
using the `labelled` class and the `label` attribute introduced in `haven` package.

## Variable labels

A variable label could be specified for any vector using `var_label`.


```r
library(labelled)

var_label(iris$Sepal.Length) <- "Length of sepal"
```

It's possible to add a variable label to several columns of a data frame using a named list.


```r
var_label(iris) <- list(Petal.Length = "Length of petal", Petal.Width = "Width of Petal")
```

To get the variable label, simply call `var_label`.


```r
var_label(iris$Petal.Width)
```

```
## [1] "Width of Petal"
```

```r
var_label(iris)
```

```
## $Sepal.Length
## [1] "Length of sepal"
## 
## $Sepal.Width
## NULL
## 
## $Petal.Length
## [1] "Length of petal"
## 
## $Petal.Width
## [1] "Width of Petal"
## 
## $Species
## NULL
```

To remove a variable label, use `NULL`.


```r
var_label(iris$Sepal.Length) <- NULL
```

In **RStudio**, variable labels will be displayed in data viewer.


```r
View(iris)
```

## Value labels

The first way to create a labelled vector is to use the `labelled` function.
It's not mandatory to provide a label for each value observed in your vector.
You can also provide a label for values not observed.


```r
v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 8, refused = 9))
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value      label
##      1        yes
##      3         no
##      8 don't know
##      9    refused
```

Use `val_labels` to get all value labels and `val_label` to get the value label associated 
with a specific value.


```r
val_labels(v)
```

```
##        yes         no don't know    refused 
##          1          3          8          9
```

```r
val_label(v, 8)
```

```
## [1] "don't know"
```

`val_labels` could also be used to modify all the value labels attached to a vector, 
while `val_label` will update only one specific value label.


```r
val_labels(v) <- c(yes = 1, nno = 3, bug = 5)
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value label
##      1   yes
##      3   nno
##      5   bug
```

```r
val_label(v, 3) <- "no"
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value label
##      1   yes
##      3    no
##      5   bug
```

With `val_label`, you can also add or remove specific value labels.


```r
val_label(v, 2) <- "maybe"
val_label(v, 5) <- NULL
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value label
##      1   yes
##      3    no
##      2 maybe
```

To remove all value labels, use `val_labels` and `NULL`. The `labelled` class will also be removed.


```r
val_labels(v) <- NULL
v
```

```
##  [1]  1  2  2  2  3  9  1  3  2 NA
```

Adding a value label to a non labelled vector will apply `labelled` class to it.


```r
val_label(v, 1) <- "yes"
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value label
##      1   yes
```

Note that applying `val_labels` to a factor will have no effect!


```r
f <- factor(1:3)
f
```

```
## [1] 1 2 3
## Levels: 1 2 3
```

```r
val_labels(f) <- c(yes = 1, no = 3)
f
```

```
## [1] 1 2 3
## Levels: 1 2 3
```

You could also apply value labels to several columns of a data frame.


```r
df <- data.frame(v1 = 1:3, v2 = c(2, 3, 1), v3 = 3:1)

val_label(df, 1) <- "yes"
val_label(df[, c("v1", "v3")], 2) <- "maybe"
val_label(df[, c("v2", "v3")], 3) <- "no"
val_labels(df)
```

```
## $v1
##   yes maybe 
##     1     2 
## 
## $v2
## yes  no 
##   1   3 
## 
## $v3
##   yes maybe    no 
##     1     2     3
```

```r
val_labels(df[, c("v1", "v3")]) <- c(YES = 1, MAYBE = 2, NO = 3)
val_labels(df)
```

```
## $v1
##   YES MAYBE    NO 
##     1     2     3 
## 
## $v2
## yes  no 
##   1   3 
## 
## $v3
##   YES MAYBE    NO 
##     1     2     3
```

```r
val_labels(df) <- NULL
val_labels(df)
```

```
## $v1
## NULL
## 
## $v2
## NULL
## 
## $v3
## NULL
```

```r
val_labels(df) <- list(v1 = c(yes = 1, no = 3), v2 = c(a = 1, b = 2, c = 3))
val_labels(df)
```

```
## $v1
## yes  no 
##   1   3 
## 
## $v2
## a b c 
## 1 2 3 
## 
## $v3
## NULL
```

## Sorting value labels

Value labels are sorted by default in the order they have been created.


```r
v <- c(1,2,2,2,3,9,1,3,2,NA)
val_label(v, 1) <- "yes"
val_label(v, 3) <- "no"
val_label(v, 9) <- "refused"
val_label(v, 2) <- "maybe"
val_label(v, 8) <- "don't know"
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value      label
##      1        yes
##      3         no
##      9    refused
##      2      maybe
##      8 don't know
```

It could be useful to reorder the value labels according to their attached values.


```r
sort_val_labels(v)
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value      label
##      1        yes
##      2      maybe
##      3         no
##      8 don't know
##      9    refused
```

```r
sort_val_labels(v, decreasing = TRUE)
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value      label
##      9    refused
##      8 don't know
##      3         no
##      2      maybe
##      1        yes
```

If you prefer, you can also sort them according to the labels.


```r
sort_val_labels(v, according_to = "l")
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value      label
##      8 don't know
##      2      maybe
##      3         no
##      9    refused
##      1        yes
```

## User defined missing values (SPSS's style)

`haven` (>= 1.0.0) introduced an additional `labelled_spss` class to deal with user defined missing values. In such case, additional atributes will be used to indicate with values should be considered as missing, but such values will not be stored as internal `NA` values. You should note that most R function will not take this information into account. Therefore, you will have to convert missing values into `NA` if required before analysis. These defined missing values could co-exist with internal `NA` values.

It is possible to manipulate this missing values with `na_values()` and `na_range()`. 


```r
v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9))
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value      label
##      1        yes
##      3         no
##      9 don't know
```

```r
na_values(v) <- 9
na_values(v)
```

```
## [1] 9
```

```r
v
```

```
## <Labelled SPSS double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## Missing values: 9
## 
## Labels:
##  value      label
##      1        yes
##      3         no
##      9 don't know
```

```r
na_values(v) <- NULL
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value      label
##      1        yes
##      3         no
##      9 don't know
```

```r
na_range(v) <- c(5, Inf)
na_range(v)
```

```
## [1]   5 Inf
```

```r
v
```

```
## <Labelled SPSS double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## Missing range:  [5, Inf]
## 
## Labels:
##  value      label
##      1        yes
##      3         no
##      9 don't know
```

It is **mandatory** to defined at least one value label before defining missing values. Therefore, the following example will fail.


```r
x <- c(1, 2, 2, 9)
na_values(x) <- 9
```

You should try:


```r
x <- c(1, 2, 2, 9)
val_labels(x) <- c(yes = 1, no = 2)
na_values(x) <- 9
x
```

```
## <Labelled SPSS double>
## [1] 1 2 2 9
## Missing values: 9
## 
## Labels:
##  value label
##      1   yes
##      2    no
```


To convert user defined missing values into `NA`, simply use `user_na_to_na()`.


```r
v <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
v
```

```
## <Labelled SPSS integer>
##  [1]  1  2  3  4  5  6  7  8  9 10
## Missing values: 9, 10
## 
## Labels:
##  value label
##      1  Good
##      8   Bad
```

```r
v2 <- user_na_to_na(v)
v2
```

```
## <Labelled integer>
##  [1]  1  2  3  4  5  6  7  8 NA NA
## 
## Labels:
##  value label
##      1  Good
##      8   Bad
```

You can also remove user missing values definition without converting these values to `NA`.


```r
v <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
v
```

```
## <Labelled SPSS integer>
##  [1]  1  2  3  4  5  6  7  8  9 10
## Missing values: 9, 10
## 
## Labels:
##  value label
##      1  Good
##      8   Bad
```

```r
v2 <- remove_user_na(v)
```

```
## Some user defined missing values have been removed but not converted to NA.
```

```r
v2
```

```
## <Labelled integer>
##  [1]  1  2  3  4  5  6  7  8  9 10
## 
## Labels:
##  value label
##      1  Good
##      8   Bad
```

or


```r
v <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
v
```

```
## <Labelled SPSS integer>
##  [1]  1  2  3  4  5  6  7  8  9 10
## Missing values: 9, 10
## 
## Labels:
##  value label
##      1  Good
##      8   Bad
```

```r
na_values(v) <- NULL
v
```

```
## <Labelled integer>
##  [1]  1  2  3  4  5  6  7  8  9 10
## 
## Labels:
##  value label
##      1  Good
##      8   Bad
```


## Other conversion to NA

In some cases, values who don't have an attached value label could be considered as missing.
`nolabel_to_na` will convert them to `NA`.


```r
v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, maybe = 2, no = 3))
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value label
##      1   yes
##      2 maybe
##      3    no
```

```r
nolabel_to_na(v)
```

```
## <Labelled double>
##  [1]  1  2  2  2  3 NA  1  3  2 NA
## 
## Labels:
##  value label
##      1   yes
##      2 maybe
##      3    no
```

In other cases, a value label is attached only to specific values that corresponds to 
a missing value. For example:


```r
size <- labelled(c(1.88, 1.62, 1.78, 99, 1.91), c("not measured" = 99))
size
```

```
## <Labelled double>
## [1]  1.88  1.62  1.78 99.00  1.91
## 
## Labels:
##  value        label
##     99 not measured
```

In such cases, `val_labels_to_na` could be appropriate.


```r
val_labels_to_na(size)
```

```
## [1] 1.88 1.62 1.78   NA 1.91
```

These two functions could also be applied to an overall data frame. 
Only labelled vectors will be impacted.

## Converting to factor

A labelled vector could easily be converted to a factor with `to_factor`.


```r
v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 8, refused = 9))
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value      label
##      1        yes
##      3         no
##      8 don't know
##      9    refused
```

```r
to_factor(v)
```

```
##  [1] yes     2       2       2       no      refused yes     no     
##  [9] 2       <NA>   
## Levels: yes 2 no don't know refused
```

The `levels` argument allows to specify what should be used as the factor levels, 
i.e. the labels (default), the values or the labels prefixed with values.


```r
to_factor(v, levels = "v")
```

```
##  [1] 1    2    2    2    3    9    1    3    2    <NA>
## Levels: 1 2 3 8 9
```

```r
to_factor(v, levels = "p")
```

```
##  [1] [1] yes     [2] 2       [2] 2       [2] 2       [3] no     
##  [6] [9] refused [1] yes     [3] no      [2] 2       <NA>       
## Levels: [1] yes [2] 2 [3] no [8] don't know [9] refused
```

The `ordered` argument will create an ordinal factor.


```r
to_factor(v, ordered = TRUE)
```

```
##  [1] yes     2       2       2       no      refused yes     no     
##  [9] 2       <NA>   
## Levels: yes < 2 < no < don't know < refused
```

The argument `nolabel_to_na` specify if the corresponding function should be 
applied before converting to a factor. Therefore, the two following commands are equivalent.


```r
to_factor(v, nolabel_to_na = TRUE)
```

```
##  [1] yes     <NA>    <NA>    <NA>    no      refused yes     no     
##  [9] <NA>    <NA>   
## Levels: yes no don't know refused
```

```r
to_factor(nolabel_to_na(v))
```

```
##  [1] yes     <NA>    <NA>    <NA>    no      refused yes     no     
##  [9] <NA>    <NA>   
## Levels: yes no don't know refused
```

`sort_levels` specifies how the levels should be sorted: `"none"` to keep the order in which value labels 
have been defined, `"values"` to order the levels according to the values and `"labels"` according 
to the labels. `"auto"` (default) will be equivalent to `"none"` except if some values with no 
attached labels are found and are not dropped. In that case, `"values"` will be used.


```r
to_factor(v, sort_levels = "n")
```

```
##  [1] yes     2       2       2       no      refused yes     no     
##  [9] 2       <NA>   
## Levels: yes no don't know refused 2
```

```r
to_factor(v, sort_levels = "v")
```

```
##  [1] yes     2       2       2       no      refused yes     no     
##  [9] 2       <NA>   
## Levels: yes 2 no don't know refused
```

```r
to_factor(v, sort_levels = "l")
```

```
##  [1] yes     2       2       2       no      refused yes     no     
##  [9] 2       <NA>   
## Levels: 2 don't know no refused yes
```

The function `to_labelled` could be used to turn a factor into a labelled numeric vector. 


```r
f <- factor(1:3, labels = c("a", "b", "c"))
to_labelled(f)
```

```
## <Labelled double>
## [1] 1 2 3
## 
## Labels:
##  value label
##      1     a
##      2     b
##      3     c
```

Note that `to_labelled(to_factor(v))` will not be equal to `v` due to the way factors are
stored internally by **R**.


```r
v
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  9  1  3  2 NA
## 
## Labels:
##  value      label
##      1        yes
##      3         no
##      8 don't know
##      9    refused
```

```r
to_labelled(to_factor(v))
```

```
## <Labelled double>
##  [1]  1  2  2  2  3  5  1  3  2 NA
## 
## Labels:
##  value      label
##      1        yes
##      2          2
##      3         no
##      4 don't know
##      5    refused
```

## Importing labelled data

In **haven** package, `read_spss`, `read_stata` and `read_sas` are natively importing data
using the `labelled` class and the `label` attribute for variable labels.

Functions from **foreign** package could also import some metadata from **SPSS** and **Stata**
files. `to_labelled` can convert data imported with **foreign** into a labelled data frame.
However, there are some limitations compared to using **haven**:

- For **SPSS** files, it will be better to set `use.value.labels = FALSE`, `to.data.frame = FALSE` and
  `use.missings = FALSE` when calling `read.spss`. If `use.value.labels = TRUE`, variable with
  value labels will be converted into factors by `read.spss` (and kept as factors by `foreign_to_label`). 
  If `to.data.frame = TRUE`, meta data describing the missing values will not be imported. 
  If `use.missings = TRUE`, missing values would have been converted to `NA` by `read.spss`.
- For **Stata** files, set `convert.factors = FALSE` when calling `read.dta` to avoid conversion
  of variables with value labels into factors. So far, missing values defined in Stata are always 
  imported as `NA` by `read.dta` and could not be retrieved by `foreign_to_labelled`.
  
The **memisc** package provide functions to import variable metadata and store them in specific
object of class `data.set`. The `to_labelled` method can convert a data.set into a labelled data frame.


```r
  # from foreign
  library(foreign)
  df <- to_labelled(read.spss(
    "file.sav",
    to.data.frame = FALSE,
    use.value.labels = FALSE,
    use.missings = FALSE
 ))
 df <- to_labelled(read.dta(
   "file.dta",
   convert.factors = FALSE
 ))

 # from memisc
 library(memisc)
 nes1948.por <- UnZip("anes/NES1948.ZIP", "NES1948.POR", package="memisc")
 nes1948 <- spss.portable.file(nes1948.por)
 df <- to_labelled(nes1948)
 ds <- as.data.set(nes19480)
 df <- to_labelled(ds)
```

## Using labelled with dplyr/magrittr

If you are using the `%>%` operator, you can use the functions `set_variable_labels`, `set_value_labels`,
`add_value_labels` and `remove_value_labels`.


```r
library(dplyr)
```

```
## 
## Attachement du package : 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
df <- data_frame(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>% 
  set_variable_labels(s1 = "Sex", s2 = "Question") %>%
  set_value_labels(s1 = c(Male = "M", Female = "F"), s2 = c(Yes = 1, No = 2))
df$s2
```

```
## <Labelled double>
## [1] 1 1 2
## 
## Labels:
##  value label
##      1   Yes
##      2    No
```

`set_value_labels` will replace the list of value labels while `add_value_labels` will update it.


```r
df <- df %>%
  set_value_labels(s2 = c(Yes = 1, "Don't know" = 8, Unknown = 9))
df$s2
```

```
## <Labelled double>
## [1] 1 1 2
## 
## Labels:
##  value      label
##      1        Yes
##      8 Don't know
##      9    Unknown
```

```r
df <- df %>%
  add_value_labels(s2 = c(No = 2))
df$s2
```

```
## <Labelled double>
## [1] 1 1 2
## 
## Labels:
##  value      label
##      1        Yes
##      8 Don't know
##      9    Unknown
##      2         No
```

You can also remove some variable and/or value labels.


```r
df <- df %>%
  set_variable_labels(s1 = NULL)

# removing one value label
df <- df %>%
  remove_value_labels(s2 = 2)
df$s2
```

```
## <Labelled double>
## [1] 1 1 2
## 
## Labels:
##  value      label
##      1        Yes
##      8 Don't know
##      9    Unknown
```

```r
# removing several value labels
df <- df %>%
  remove_value_labels(s2 = 8:9)
df$s2
```

```
## <Labelled double>
## [1] 1 1 2
## 
## Labels:
##  value label
##      1   Yes
```

```r
# removing all value labels
df <- df %>%
  set_value_labels(s2 = NULL)
df$s2
```

```
## [1] 1 1 2
## attr(,"label")
## [1] "Question"
```

