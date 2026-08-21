# Convert input to a character vector

By default, `to_character()` is a wrapper for
[`base::as.character()`](https://rdrr.io/r/base/character.html). For
labelled vector, to_character allows to specify if value, labels or
labels prefixed with values should be used for conversion.

## Usage

``` r
to_character(x, ...)

# S3 method for class 'double'
to_character(x, explicit_tagged_na = FALSE, ...)

# S3 method for class 'haven_labelled'
to_character(
  x,
  levels = c("labels", "values", "prefixed"),
  nolabel_to_na = FALSE,
  user_na_to_na = FALSE,
  explicit_tagged_na = FALSE,
  ...
)

# S3 method for class 'data.frame'
to_character(
  x,
  levels = c("labels", "values", "prefixed"),
  nolabel_to_na = FALSE,
  user_na_to_na = FALSE,
  explicit_tagged_na = FALSE,
  labelled_only = TRUE,
  ...
)

# S3 method for class 'survey.design'
to_character(
  x,
  levels = c("labels", "values", "prefixed"),
  nolabel_to_na = FALSE,
  user_na_to_na = FALSE,
  explicit_tagged_na = FALSE,
  labelled_only = TRUE,
  ...
)
```

## Arguments

- x:

  Object to coerce to a character vector.

- ...:

  Other arguments passed down to method.

- explicit_tagged_na:

  should tagged NA be kept?

- levels:

  What should be used for the factor levels: the labels, the values or
  labels prefixed with values?

- nolabel_to_na:

  Should values with no label be converted to `NA`?

- user_na_to_na:

  user defined missing values into NA?

- labelled_only:

  for a data.frame, convert only labelled variables to factors?

## Details

If some values doesn't have a label, automatic labels will be created,
except if `nolabel_to_na` is `TRUE`.

When applied to a data.frame, only labelled vectors are converted by
default to character. Use `labelled_only = FALSE` to convert all
variables to characters.

## Examples

``` r
v <- labelled(
  c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
  c(yes = 1, no = 3, "don't know" = 9)
)
to_character(v)
#>  [1] "yes"        "2"          "2"          "2"          "no"        
#>  [6] "don't know" "yes"        "no"         "2"          NA          
to_character(v, nolabel_to_na = TRUE)
#>  [1] "yes"        NA           NA           NA           "no"        
#>  [6] "don't know" "yes"        "no"         NA           NA          
to_character(v, "v")
#>  [1] "1" "2" "2" "2" "3" "9" "1" "3" "2" NA 
to_character(v, "p")
#>  [1] "[1] yes"        "[2] 2"          "[2] 2"          "[2] 2"         
#>  [5] "[3] no"         "[9] don't know" "[1] yes"        "[3] no"        
#>  [9] "[2] 2"          NA              

df <- data.frame(
  a = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2)),
  b = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2, DK = 3)),
  c = labelled(
    c("a", "a", "b", "c"),
    labels = c(No = "a", Maybe = "b", Yes = "c")
  ),
  d = 1:4,
  e = factor(c("item1", "item2", "item1", "item2")),
  f = c("itemA", "itemA", "itemB", "itemB"),
  stringsAsFactors = FALSE
)

if (require(dplyr)) {
  glimpse(df)
  glimpse(to_character(df))
  glimpse(to_character(df, labelled_only = FALSE))
}
#> Rows: 4
#> Columns: 6
#> $ a <dbl+lbl> 1, 1, 2, 3
#> $ b <dbl+lbl> 1, 1, 2, 3
#> $ c <chr+lbl> "a", "a", "b", "c"
#> $ d <int> 1, 2, 3, 4
#> $ e <fct> item1, item2, item1, item2
#> $ f <chr> "itemA", "itemA", "itemB", "itemB"
#> Rows: 4
#> Columns: 6
#> $ a <chr> "No", "No", "Yes", "3"
#> $ b <chr> "No", "No", "Yes", "DK"
#> $ c <chr> "No", "No", "Maybe", "Yes"
#> $ d <int> 1, 2, 3, 4
#> $ e <fct> item1, item2, item1, item2
#> $ f <chr> "itemA", "itemA", "itemB", "itemB"
#> Rows: 4
#> Columns: 6
#> $ a <chr> "No", "No", "Yes", "3"
#> $ b <chr> "No", "No", "Yes", "DK"
#> $ c <chr> "No", "No", "Maybe", "Yes"
#> $ d <chr> "1", "2", "3", "4"
#> $ e <chr> "item1", "item2", "item1", "item2"
#> $ f <chr> "itemA", "itemA", "itemB", "itemB"
```
