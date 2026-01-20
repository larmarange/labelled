# Remove variable label, value labels and user defined missing values

Use `remove_var_label()` to remove variable label, `remove_val_labels()`
to remove value labels, `remove_user_na()` to remove user defined
missing values (*na_values* and *na_range*) and `remove_labels()` to
remove all.

## Usage

``` r
remove_labels(
  x,
  user_na_to_na = FALSE,
  keep_var_label = FALSE,
  user_na_to_tagged_na = FALSE
)

remove_var_label(x)

remove_val_labels(x)

remove_user_na(x, user_na_to_na = FALSE, user_na_to_tagged_na = FALSE)
```

## Arguments

- x:

  A vector, a data frame or a survey design.

- user_na_to_na:

  Convert user defined missing values into `NA`?

- keep_var_label:

  Keep variable label?

- user_na_to_tagged_na:

  Convert user defined missing values into tagged `NA`? It could be
  applied only to numeric vectors. Note that integer labelled vectors
  will be converted to double labelled vectors.

## Details

Be careful with `remove_user_na()` and `remove_labels()`, user defined
missing values will not be automatically converted to `NA`, except if
you specify `user_na_to_na = TRUE`. `user_na_to_na(x)` is an equivalent
of `remove_user_na(x, user_na_to_na = TRUE)`.

If you prefer to convert variables with value labels into factors, use
[`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
or use
[`unlabelled()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md).

## Examples

``` r
x <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
var_label(x) <- "A variable"
x
#> <labelled_spss<integer>[10]>: A variable
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> Missing values: 9, 10
#> 
#> Labels:
#>  value label
#>      1  Good
#>      8   Bad

remove_labels(x)
#>  [1]  1  2  3  4  5  6  7  8  9 10
remove_labels(x, user_na_to_na = TRUE)
#>  [1]  1  2  3  4  5  6  7  8 NA NA
remove_user_na(x, user_na_to_na = TRUE)
#> <labelled<integer>[10]>: A variable
#>  [1]  1  2  3  4  5  6  7  8 NA NA
#> 
#> Labels:
#>  value label
#>      1  Good
#>      8   Bad
remove_user_na(x, user_na_to_tagged_na = TRUE)
#> ℹ `x` has been converted into a double vector.
#> <labelled<double>[10]>: A variable
#>  [1]     1     2     3     4     5     6     7     8 NA(a) NA(b)
#> 
#> Labels:
#>  value label
#>      1  Good
#>      8   Bad
```
