# Convert input to a factor.

The base function
[`base::as.factor()`](https://rdrr.io/r/base/factor.html) is not a
generic, but this variant is. By default, `to_factor()` is a wrapper for
[`base::as.factor()`](https://rdrr.io/r/base/factor.html). Please note
that `to_factor()` differs slightly from
[`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)
method provided by haven package.

`unlabelled(x)` is a shortcut for
`to_factor(x, strict = TRUE, unclass = TRUE, labelled_only = TRUE)`.

## Usage

``` r
to_factor(x, ...)

# S3 method for class 'haven_labelled'
to_factor(
  x,
  levels = c("labels", "values", "prefixed"),
  ordered = FALSE,
  nolabel_to_na = FALSE,
  sort_levels = c("auto", "none", "labels", "values"),
  decreasing = FALSE,
  drop_unused_labels = FALSE,
  user_na_to_na = FALSE,
  strict = FALSE,
  unclass = FALSE,
  explicit_tagged_na = FALSE,
  ...
)

# S3 method for class 'data.frame'
to_factor(
  x,
  levels = c("labels", "values", "prefixed"),
  ordered = FALSE,
  nolabel_to_na = FALSE,
  sort_levels = c("auto", "none", "labels", "values"),
  decreasing = FALSE,
  labelled_only = TRUE,
  drop_unused_labels = FALSE,
  user_na_to_na = FALSE,
  strict = FALSE,
  unclass = FALSE,
  explicit_tagged_na = FALSE,
  ...
)

# S3 method for class 'survey.design'
to_factor(
  x,
  levels = c("labels", "values", "prefixed"),
  ordered = FALSE,
  nolabel_to_na = FALSE,
  sort_levels = c("auto", "none", "labels", "values"),
  decreasing = FALSE,
  labelled_only = TRUE,
  drop_unused_labels = FALSE,
  user_na_to_na = FALSE,
  strict = FALSE,
  unclass = FALSE,
  explicit_tagged_na = FALSE,
  ...
)

unlabelled(x, ...)
```

## Arguments

- x:

  Object to coerce to a factor.

- ...:

  Other arguments passed down to method.

- levels:

  What should be used for the factor levels: the labels, the values or
  labels prefixed with values?

- ordered:

  `TRUE` for ordinal factors, `FALSE` (default) for nominal factors.

- nolabel_to_na:

  Should values with no label be converted to `NA`?

- sort_levels:

  How the factor levels should be sorted? (see Details)

- decreasing:

  Should levels be sorted in decreasing order?

- drop_unused_labels:

  Should unused value labels be dropped? (applied only if
  `strict = FALSE`)

- user_na_to_na:

  Convert user defined missing values into `NA`?

- strict:

  Convert to factor only if all values have a defined label?

- unclass:

  If not converted to a factor (when `strict = TRUE`), convert to a
  character or a numeric factor by applying
  [`base::unclass()`](https://rdrr.io/r/base/class.html)?

- explicit_tagged_na:

  Should tagged NA (cf.
  [`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html))
  be kept as explicit factor levels?

- labelled_only:

  for a data.frame, convert only labelled variables to factors?

## Details

If some values doesn't have a label, automatic labels will be created,
except if `nolabel_to_na` is `TRUE`.

If `sort_levels == 'values'`, the levels will be sorted according to the
values of `x`. If `sort_levels == 'labels'`, the levels will be sorted
according to labels' names. If `sort_levels == 'none'`, the levels will
be in the order the value labels are defined in `x`. If some labels are
automatically created, they will be added at the end. If
`sort_levels == 'auto'`, `sort_levels == 'none'` will be used, except if
some values doesn't have a defined label. In such case,
`sort_levels == 'values'` will be applied.

When applied to a data.frame, only labelled vectors are converted by
default to a factor. Use `labelled_only = FALSE` to convert all
variables to factors.

`unlabelled()` is a shortcut for quickly removing value labels of a
vector or of a data.frame. If all observed values have a value label,
then the vector will be converted into a factor. Otherwise, the vector
will be unclassed. If you want to remove value labels in all cases, use
[`remove_val_labels()`](https://larmarange.github.io/labelled/reference/remove_labels.md).

## Examples

``` r
v <- labelled(
  c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
  c(yes = 1, no = 3, "don't know" = 9)
)
to_factor(v)
#>  [1] yes        2          2          2          no         don't know
#>  [7] yes        no         2          <NA>      
#> Levels: yes 2 no don't know
to_factor(v, nolabel_to_na = TRUE)
#>  [1] yes        <NA>       <NA>       <NA>       no         don't know
#>  [7] yes        no         <NA>       <NA>      
#> Levels: yes no don't know
to_factor(v, "p")
#>  [1] [1] yes        [2] 2          [2] 2          [2] 2          [3] no        
#>  [6] [9] don't know [1] yes        [3] no         [2] 2          <NA>          
#> Levels: [1] yes [2] 2 [3] no [9] don't know
to_factor(v, sort_levels = "v")
#>  [1] yes        2          2          2          no         don't know
#>  [7] yes        no         2          <NA>      
#> Levels: yes 2 no don't know
to_factor(v, sort_levels = "n")
#>  [1] yes        2          2          2          no         don't know
#>  [7] yes        no         2          <NA>      
#> Levels: yes no don't know 2
to_factor(v, sort_levels = "l")
#>  [1] yes        2          2          2          no         don't know
#>  [7] yes        no         2          <NA>      
#> Levels: 2 don't know no yes

x <- labelled(c("H", "M", "H", "L"), c(low = "L", medium = "M", high = "H"))
to_factor(x, ordered = TRUE)
#> [1] high   medium high   low   
#> Levels: low < medium < high

# Strict conversion
v <- labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2))
to_factor(v)
#> [1] No  No  Yes 3  
#> Levels: No Yes 3
to_factor(v, strict = TRUE) # Not converted because 3 does not have a label
#> <labelled<double>[4]>
#> [1] 1 1 2 3
#> 
#> Labels:
#>  value label
#>      1    No
#>      2   Yes
to_factor(v, strict = TRUE, unclass = TRUE)
#> [1] 1 1 2 3
#> attr(,"labels")
#>  No Yes 
#>   1   2 

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
  glimpse(unlabelled(df))
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
#> $ a <dbl> 1, 1, 2, 3
#> $ b <fct> No, No, Yes, DK
#> $ c <fct> No, No, Maybe, Yes
#> $ d <int> 1, 2, 3, 4
#> $ e <fct> item1, item2, item1, item2
#> $ f <chr> "itemA", "itemA", "itemB", "itemB"
```
