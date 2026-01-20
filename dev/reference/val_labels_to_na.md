# Recode value labels to NA

For labelled variables, values with a label will be recoded to `NA`.

## Usage

``` r
val_labels_to_na(x)
```

## Arguments

- x:

  Object to recode.

## See also

[`haven::zap_labels()`](https://haven.tidyverse.org/reference/zap_labels.html)

## Examples

``` r
v <- labelled(c(1, 2, 9, 1, 9), c(dk = 9))
val_labels_to_na(v)
#> [1]  1  2 NA  1 NA
```
