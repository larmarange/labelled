# Get / Set SPSS missing values

Get / Set SPSS missing values

## Usage

``` r
na_values(x)

na_values(x) <- value

na_range(x)

na_range(x) <- value

get_na_values(x)

get_na_range(x)

set_na_values(.data, ..., .values = NA, .strict = TRUE)

set_na_range(.data, ..., .values = NA, .strict = TRUE)

is_user_na(x)

is_regular_na(x)

user_na_to_na(x)

user_na_to_regular_na(x)

user_na_to_tagged_na(x)
```

## Arguments

- x:

  A vector, a data frame, or a survey design.

- value:

  A vector of values that should also be considered as missing (for
  `na_values`) or a numeric vector of length two giving the (inclusive)
  extents of the range (for `na_range`), use `-Inf` and `Inf` if you
  want the range to be open ended).

- .data:

  a data frame, a survey design or a vector

- ...:

  name-value pairs of missing values (see examples)

- .values:

  missing values to be applied to the data.frame, using the same syntax
  as `value` in `na_values(df) <- value` or `na_range(df) <- value`.

- .strict:

  should an error be returned if some labels doesn't correspond to a
  column of `x`?

## Value

`na_values()` will return a vector of values that should also be
considered as missing. `na_range()` will return a numeric vector of
length two giving the (inclusive) extents of the range.

`set_na_values()` and `set_na_range()` will return an updated copy of
`.data`.

## Details

See
[`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
for a presentation of SPSS's user defined missing values.

Note that [`base::is.na()`](https://rdrr.io/r/base/NA.html) will return
`TRUE` for user defined missing values. It will also return `TRUE` for
regular `NA` values. If you want to test if a specific value is a user
NA but not a regular `NA`, use `is_user_na()`. If you want to test if a
value is a regular `NA` but not a user NA, not a tagged NA, use
`is_regular_na()`.

You can use `user_na_to_na()` to convert user defined missing values to
regular `NA`. Note that any value label attached to a user defined
missing value will be lost. `user_na_to_regular_na()` is a synonym of
`user_na_to_na()`.

The method `user_na_to_tagged_na()` will convert user defined missing
values into
[`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html),
preserving value labels. Please note that
[`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
are defined only for double vectors. Therefore, integer
`haven_labelled_spss` vectors will be converted into double
`haven_labelled` vectors; and `user_na_to_tagged_na()` cannot be applied
to a character `haven_labelled_spss` vector.

[`tagged_na_to_user_na()`](https://larmarange.github.io/labelled/dev/reference/tagged_na_to_user_na.md)
is the opposite of `user_na_to_tagged_na()` and convert tagged `NA` into
user defined missing values.

## Note

`get_na_values()` is identical to `na_values()` and `get_na_range()` to
`na_range()`.

`set_na_values()` and `set_na_range()` could be used with dplyr syntax.

## See also

[`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html),
`user_na_to_na()`

## Examples

``` r
v <- labelled(
  c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
  c(yes = 1, no = 3, "don't know" = 9)
)
v
#> <labelled<double>[10]>
#>  [1]  1  2  2  2  3  9  1  3  2 NA
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      3         no
#>      9 don't know
na_values(v) <- 9
na_values(v)
#> [1] 9
v
#> <labelled_spss<double>[10]>
#>  [1]  1  2  2  2  3  9  1  3  2 NA
#> Missing values: 9
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      3         no
#>      9 don't know

is.na(v) # TRUE for the 6th and 10th values
#>  [1] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
is_user_na(v) # TRUE only for the 6th value
#>  [1] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE

user_na_to_na(v)
#> <labelled<double>[10]>
#>  [1]  1  2  2  2  3 NA  1  3  2 NA
#> 
#> Labels:
#>  value label
#>      1   yes
#>      3    no
na_values(v) <- NULL
v
#> <labelled<double>[10]>
#>  [1]  1  2  2  2  3  9  1  3  2 NA
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      3         no
#>      9 don't know
na_range(v) <- c(5, Inf)
na_range(v)
#> [1]   5 Inf
v
#> <labelled_spss<double>[10]>
#>  [1]  1  2  2  2  3  9  1  3  2 NA
#> Missing range:  [5, Inf]
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      3         no
#>      9 don't know
user_na_to_na(v)
#> <labelled<double>[10]>
#>  [1]  1  2  2  2  3 NA  1  3  2 NA
#> 
#> Labels:
#>  value label
#>      1   yes
#>      3    no
user_na_to_tagged_na(v)
#> <labelled<double>[10]>
#>  [1]     1     2     2     2     3 NA(a)     1     3     2    NA
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      3         no
#>  NA(a) don't know

# it is not recommended to mix user NAs and tagged NAs
x <- c(NA, 9, tagged_na("a"))
na_values(x) <- 9
x
#> <labelled_spss<double>[3]>
#> [1]    NA     9 NA(a)
#> Missing values: 9
is.na(x)
#> [1] TRUE TRUE TRUE
is_user_na(x)
#> [1] FALSE  TRUE FALSE
is_tagged_na(x)
#> [1] FALSE FALSE  TRUE
is_regular_na(x)
#> [1]  TRUE FALSE FALSE

if (require(dplyr)) {
  # setting value label and user NAs
  df <- tibble(s1 = c("M", "M", "F", "F"), s2 = c(1, 1, 2, 9)) %>%
    set_value_labels(s2 = c(yes = 1, no = 2)) %>%
    set_na_values(s2 = 9)
  na_values(df)

  # removing missing values
  df <- df %>% set_na_values(s2 = NULL)
  df$s2

  # example with a vector
  v <- 1:10
  v <- v %>% set_na_values(5, 6, 7)
  v
  v %>% set_na_range(8, 10)
  v %>% set_na_range(.values = c(9, 10))
  v %>% set_na_values(NULL)
}
#> <labelled<integer>[10]>
#>  [1]  1  2  3  4  5  6  7  8  9 10
```
