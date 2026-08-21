# Copy variable and value labels and SPSS-style missing value

This function copies variable and value labels (including missing
values) from one vector to another or from one data frame to another
data frame. For data frame, labels are copied according to variable
names, and only if variables are the same type in both data frames.

## Usage

``` r
copy_labels(from, to, .strict = TRUE)

copy_labels_from(to, from, .strict = TRUE)
```

## Arguments

- from:

  A vector or a data.frame (or survey design) to copy labels from.

- to:

  A vector or data.frame (or survey design) to copy labels to.

- .strict:

  When `from` is a labelled vector, `to` have to be of the same type
  (numeric or character) in order to copy value labels and SPSS-style
  missing values. If this is not the case and `.strict = TRUE`, an error
  will be produced. If `.strict = FALSE`, only variable label will be
  copied.

## Details

Some base R functions like
[`base::subset()`](https://rdrr.io/r/base/subset.html) drop variable and
value labels attached to a variable. `copy_labels` could be used to
restore these attributes.

`copy_labels_from` is intended to be used with dplyr syntax, see
examples.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
df <- tibble(
  id = 1:3,
  happy = factor(c("yes", "no", "yes")),
  gender = labelled(c(1, 1, 2), c(female = 1, male = 2))
) %>%
  set_variable_labels(
    id = "Individual ID",
    happy = "Are you happy?",
    gender = "Gender of respondent"
  )
var_label(df)
#> $id
#> [1] "Individual ID"
#> 
#> $happy
#> [1] "Are you happy?"
#> 
#> $gender
#> [1] "Gender of respondent"
#> 
fdf <- df %>% filter(id < 3)
var_label(fdf) # some variable labels have been lost
#> $id
#> [1] "Individual ID"
#> 
#> $happy
#> [1] "Are you happy?"
#> 
#> $gender
#> [1] "Gender of respondent"
#> 
fdf <- fdf %>% copy_labels_from(df)
var_label(fdf)
#> $id
#> [1] "Individual ID"
#> 
#> $happy
#> [1] "Are you happy?"
#> 
#> $gender
#> [1] "Gender of respondent"
#> 

# Alternative syntax
fdf <- subset(df, id < 3)
fdf <- copy_labels(from = df, to = fdf)
```
