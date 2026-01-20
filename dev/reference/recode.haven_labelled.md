# Recode values

Extend
[`dplyr::recode()`](https://dplyr.tidyverse.org/reference/recode.html)
method from dplyr to works with labelled vectors.

## Usage

``` r
# S3 method for class 'haven_labelled'
recode(
  .x,
  ...,
  .default = NULL,
  .missing = NULL,
  .keep_value_labels = TRUE,
  .combine_value_labels = FALSE,
  .sep = " / "
)
```

## Arguments

- .x:

  A vector to modify

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Replacements. For character and factor `.x`, these should be named and
  replacement is based only on their name. For numeric `.x`, these can
  be named or not. If not named, the replacement is done based on
  position i.e. `.x` represents positions to look for in replacements.
  See examples.

  When named, the argument names should be the current values to be
  replaced, and the argument values should be the new (replacement)
  values.

  All replacements must be the same type, and must have either length
  one or the same length as `.x`.

- .default:

  If supplied, all values not otherwise matched will be given this
  value. If not supplied and if the replacements are the same type as
  the original values in `.x`, unmatched values are not changed. If not
  supplied and if the replacements are not compatible, unmatched values
  are replaced with `NA`.

  `.default` must be either length 1 or the same length as `.x`.

- .missing:

  If supplied, any missing values in `.x` will be replaced by this
  value. Must be either length 1 or the same length as `.x`.

- .keep_value_labels:

  If `TRUE`, keep original value labels. If `FALSE`, remove value
  labels.

- .combine_value_labels:

  If `TRUE`, will combine original value labels to generate new value
  labels. Note that unexpected results could be obtained if a same old
  value is recoded into several different new values.

- .sep:

  Separator to be used when combining value labels.

## See also

[`dplyr::recode()`](https://dplyr.tidyverse.org/reference/recode.html)

## Examples

``` r
x <- labelled(1:3, c(yes = 1, no = 2))
x
#> <labelled<integer>[3]>
#> [1] 1 2 3
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no
dplyr::recode(x, `3` = 2L)
#> <labelled<integer>[3]>
#> [1] 1 2 2
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no

# do not keep value labels
dplyr::recode(x, `3` = 2L, .keep_value_labels = FALSE)
#> [1] 1 2 2

# be careful, changes are not of the same type (here integers),
# NA arecreated
dplyr::recode(x, `3` = 2)
#> Warning: Unreplaced values treated as NA as `.x` is not compatible.
#> Please specify replacements exhaustively or supply `.default`.
#> <labelled<double>[3]>
#> [1] NA NA  2
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no

# except if you provide .default or new values for all old values
dplyr::recode(x, `1` = 1, `2` = 1, `3` = 2)
#> <labelled<double>[3]>
#> [1] 1 1 2
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no

# if you change the type of the vector (here transformed into character)
# value labels are lost
dplyr::recode(x, `3` = "b", .default = "a")
#> Warning: The type of `.x` (character) has been changed and value labels have been lost.
#> [1] "a" "a" "b"

# use .keep_value_labels = FALSE to avoid a warning
dplyr::recode(x, `3` = "b", .default = "a", .keep_value_labels = FALSE)
#> [1] "a" "a" "b"

# combine value labels
x <- labelled(
  1:4,
  c(
    "strongly agree" = 1,
    "agree" = 2,
    "disagree" = 3,
    "strongly disagree" = 4
  )
)
dplyr::recode(
  x,
  `1` = 1L,
  `2` = 1L,
  `3` = 2L,
  `4` = 2L,
  .combine_value_labels = TRUE
)
#> <labelled<integer>[4]>
#> [1] 1 1 2 2
#> 
#> Labels:
#>  value                        label
#>      1       strongly agree / agree
#>      2 disagree / strongly disagree
dplyr::recode(
  x,
  `2` = 1L,
  `4` = 3L,
  .combine_value_labels = TRUE
)
#> <labelled<integer>[4]>
#> [1] 1 1 3 3
#> 
#> Labels:
#>  value                        label
#>      1       strongly agree / agree
#>      3 disagree / strongly disagree
dplyr::recode(
  x,
  `2` = 1L,
  `4` = 3L,
  .combine_value_labels = TRUE,
  .sep = " or "
)
#> <labelled<integer>[4]>
#> [1] 1 1 3 3
#> 
#> Labels:
#>  value                         label
#>      1       strongly agree or agree
#>      3 disagree or strongly disagree
dplyr::recode(
  x,
  `2` = 1L,
  .default = 2L,
  .combine_value_labels = TRUE
)
#> <labelled<integer>[4]>
#> [1] 2 1 2 2
#> 
#> Labels:
#>  value                                         label
#>      1                                         agree
#>      2 strongly agree / disagree / strongly disagree

# example when combining some values without a label
y <- labelled(1:4, c("strongly agree" = 1))
dplyr::recode(y, `2` = 1L, `4` = 3L, .combine_value_labels = TRUE)
#> <labelled<integer>[4]>
#> [1] 1 1 3 3
#> 
#> Labels:
#>  value          label
#>      1 strongly agree
```
