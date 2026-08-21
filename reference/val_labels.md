# Get / Set value labels

Get / Set value labels

## Usage

``` r
val_labels(x, prefixed = FALSE)

val_labels(x, null_action = c("unclass", "labelled")) <- value

val_label(x, v, prefixed = FALSE)

val_label(x, v, null_action = c("unclass", "labelled")) <- value

get_value_labels(x, prefixed = FALSE)

set_value_labels(
  .data,
  ...,
  .labels = NA,
  .strict = TRUE,
  .overwrite = TRUE,
  .null_action = c("unclass", "labelled")
)

add_value_labels(
  .data,
  ...,
  .strict = TRUE,
  .null_action = c("unclass", "labelled")
)

remove_value_labels(
  .data,
  ...,
  .strict = TRUE,
  .null_action = c("unclass", "labelled")
)
```

## Arguments

- x:

  A vector, a data frame or a survey design

- prefixed:

  Should labels be prefixed with values?

- null_action, .null_action:

  for advanced users, if `value = NULL`, unclass the vector (default) or
  force/keep `haven_labelled` class (if `null_action = "labelled"`)

- value:

  A named vector for `val_labels()` (see
  [`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html))
  or a character string for `val_label()`. `NULL` to remove the labels
  (except if `null_action = "labelled"`). For data frames and survey
  design, it could also be a named list with a vector of value labels
  per variable.

- v:

  A single value.

- .data:

  a data frame or a vector

- ...:

  name-value pairs of value labels (see examples)

- .labels:

  value labels to be applied to the data.frame, using the same syntax as
  `value` in `val_labels(df) <- value`.

- .strict:

  should an error be returned if some labels doesn't correspond to a
  column of `x`?

- .overwrite:

  If value labels are already defined, overwrite them?

## Value

`val_labels()` will return a named vector. `val_label()` will return a
single character string.

`set_value_labels()`, `add_value_labels()` and `remove_value_labels()`
will return an updated copy of `.data`.

## Note

`get_value_labels()` is identical to `val_labels()`.

`set_value_labels()`, `add_value_labels()` and `remove_value_labels()`
could be used with dplyr syntax. While `set_value_labels()` will replace
the list of value labels, `add_value_labels()` and
`remove_value_labels()` will update that list (see examples).

`set_value_labels()` could also be applied to a vector / a data.frame
column. In such case, you can provide a vector of value labels using
`.labels` or several name-value pairs of value labels (see example). You
could consider
[`dictionary_to_value_labels()`](https://larmarange.github.io/labelled/reference/dictionary_to_variable_labels.md)
to generate a list of variable labels from a dictionary data frame.
Similarly, `add_value_labels()` and `remove_value_labels()` could also
be applied on vectors.

## Examples

``` r
v <- labelled(
  c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
  c(yes = 1, no = 3, "don't know" = 9)
)
val_labels(v)
#>        yes         no don't know 
#>          1          3          9 
val_labels(v, prefixed = TRUE)
#>        [1] yes         [3] no [9] don't know 
#>              1              3              9 
val_label(v, 2)
#> NULL
val_label(v, 2) <- "maybe"
v
#> <labelled<double>[10]>
#>  [1]  1  2  2  2  3  9  1  3  2 NA
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      3         no
#>      9 don't know
#>      2      maybe
val_label(v, 9) <- NULL
v
#> <labelled<double>[10]>
#>  [1]  1  2  2  2  3  9  1  3  2 NA
#> 
#> Labels:
#>  value label
#>      1   yes
#>      3    no
#>      2 maybe
val_labels(v, null_action = "labelled") <- NULL
v
#> <labelled<double>[10]>
#>  [1]  1  2  2  2  3  9  1  3  2 NA
val_labels(v) <- NULL
v
#>  [1]  1  2  2  2  3  9  1  3  2 NA
if (require(dplyr)) {
  # setting value labels
  df <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>%
    set_value_labels(
      s1 = c(Male = "M", Female = "F"),
      s2 = c(Yes = 1, No = 2)
    )
  val_labels(df)

  # updating value labels
  df <- df %>% add_value_labels(s2 = c(Unknown = 9))
  df$s2

  # removing a value labels
  df <- df %>% remove_value_labels(s2 = 9)
  df$s2

  # removing all value labels
  df <- df %>% set_value_labels(s2 = NULL)
  df$s2

  # example on a vector
  v <- 1:4
  v <- set_value_labels(v, min = 1, max = 4)
  v
  v %>% set_value_labels(middle = 3)
  v %>% set_value_labels(NULL)
  v %>% set_value_labels(.labels = c(a = 1, b = 2, c = 3, d = 4))
  v %>% add_value_labels(between = 2)
  v %>% remove_value_labels(4)
}
#> <labelled<integer>[4]>
#> [1] 1 2 3 4
#> 
#> Labels:
#>  value label
#>      1   min
```
