# Get / Set a variable label

Get / Set a variable label

## Usage

``` r
var_label(x, ...)

# S3 method for class 'data.frame'
var_label(
  x,
  unlist = FALSE,
  null_action = c("keep", "fill", "skip", "na", "empty"),
  recurse = FALSE,
  ...
)

# S3 method for class 'survey.design'
var_label(
  x,
  unlist = FALSE,
  null_action = c("keep", "fill", "skip", "na", "empty"),
  recurse = FALSE,
  ...
)

var_label(x) <- value

get_variable_labels(x, ...)

set_variable_labels(
  .data,
  ...,
  .labels = NA,
  .strict = TRUE,
  .overwrite = TRUE
)

label_attribute(x)

get_label_attribute(x)

set_label_attribute(x, value, .overwrite = TRUE)

label_attribute(x) <- value
```

## Arguments

- x:

  a vector or a data.frame

- ...:

  name-value pairs of variable labels (see examples)

- unlist:

  for data frames and survey design, return a named vector instead of a
  list

- null_action:

  for data frames and survey design, by default `NULL` will be returned
  for columns with no variable label. Use `"fill"` to populate with the
  column name instead, `"skip"` to remove such values from the returned
  list, `"na"` to populate with `NA` or `"empty"` to populate with an
  empty string (`""`).

- recurse:

  if `TRUE`, will apply `var_label()` on packed columns (see
  [`tidyr::pack()`](https://tidyr.tidyverse.org/reference/pack.html)) to
  return the variable labels of each sub-column; otherwise, the label of
  the group of columns will be returned.

- value:

  a character string or `NULL` to remove the label For data frames, with
  `var_label()`, it could also be a named list or a character vector of
  same length as the number of columns in `x`.

- .data:

  a data frame, a survey design or a vector

- .labels:

  variable labels to be applied to the data.frame, using the same syntax
  as `value` in `var_label(df) <- value`.

- .strict:

  should an error be returned if some labels doesn't correspond to a
  column of `x`?

- .overwrite:

  if a variable label is already defined, overwrite it?

## Value

`set_variable_labels()` will return an updated copy of `.data`.

## Details

`get_variable_labels()` is identical to `var_label()`.

For data frames and survey design, if you are using `var_label()<-` and
if `value` is a named list, only elements whose name will match a column
of the data frame will be taken into account. If `value` is a character
vector, labels should be in the same order as the columns of the data
frame / survey design. You could consider
[`dictionary_to_variable_labels()`](https://larmarange.github.io/labelled/reference/dictionary_to_variable_labels.md)
to generate a list of variable labels from a dictionary data frame.

If you are using `label_attribute()<-` or `set_label_attribute()` on a
data frame, the label attribute will be attached to the data frame
itself, not to a column of the data frame.

If you are using packed columns (see
[`tidyr::pack()`](https://tidyr.tidyverse.org/reference/pack.html)),
please read the dedicated vignette.

## Note

`set_variable_labels()` could be used with dplyr syntax.

## Examples

``` r
var_label(iris$Sepal.Length)
#> NULL
var_label(iris$Sepal.Length) <- "Length of the sepal"
if (FALSE) { # \dontrun{
View(iris)
} # }
# To remove a variable label
var_label(iris$Sepal.Length) <- NULL
# To change several variable labels at once
var_label(iris) <- c(
  "sepal length", "sepal width", "petal length",
  "petal width", "species"
)
var_label(iris)
#> $Sepal.Length
#> [1] "sepal length"
#> 
#> $Sepal.Width
#> [1] "sepal width"
#> 
#> $Petal.Length
#> [1] "petal length"
#> 
#> $Petal.Width
#> [1] "petal width"
#> 
#> $Species
#> [1] "species"
#> 
var_label(iris) <- list(
  Petal.Width = "width of the petal",
  Petal.Length = "length of the petal",
  Sepal.Width = NULL,
  Sepal.Length = NULL
)
var_label(iris)
#> $Sepal.Length
#> NULL
#> 
#> $Sepal.Width
#> NULL
#> 
#> $Petal.Length
#> [1] "length of the petal"
#> 
#> $Petal.Width
#> [1] "width of the petal"
#> 
#> $Species
#> [1] "species"
#> 
var_label(iris, null_action = "fill")
#> $Sepal.Length
#> [1] "Sepal.Length"
#> 
#> $Sepal.Width
#> [1] "Sepal.Width"
#> 
#> $Petal.Length
#> [1] "length of the petal"
#> 
#> $Petal.Width
#> [1] "width of the petal"
#> 
#> $Species
#> [1] "species"
#> 
var_label(iris, null_action = "skip")
#> $Petal.Length
#> [1] "length of the petal"
#> 
#> $Petal.Width
#> [1] "width of the petal"
#> 
#> $Species
#> [1] "species"
#> 
var_label(iris, unlist = TRUE)
#>          Sepal.Length           Sepal.Width          Petal.Length 
#>                    ""                    "" "length of the petal" 
#>           Petal.Width               Species 
#>  "width of the petal"             "species" 

#
if (require(dplyr)) {
  # adding some variable labels
  df <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>%
    set_variable_labels(s1 = "Sex", s2 = "Yes or No?")
  var_label(df)

  # removing a variable label
  df <- df %>% set_variable_labels(s2 = NULL)
  var_label(df$s2)

  # Set labels from dictionary, e.g. as read from external file
  # One description is missing, one has no match
  description <- tibble(
    name = c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Something"
    ),
    label = c(
      "Sepal length", "Sepal width", "Petal length", "Petal width",
      "something"
    )
  )
  var_labels <- stats::setNames(as.list(description$label), description$name)
  iris_labelled <- iris %>%
    set_variable_labels(.labels = var_labels, .strict = FALSE)
  var_label(iris_labelled)

  # defining variable labels derived from variable names
  if (require(snakecase)) {
    iris <- iris %>%
      set_variable_labels(.labels = to_sentence_case(names(iris)))
    var_label(iris)
  }

  # example with a vector
  v <- 1:5
  v <- v %>% set_variable_labels("a variable label")
  v
  v %>% set_variable_labels(NULL)
}
#> Loading required package: snakecase
#> [1] 1 2 3 4 5
```
