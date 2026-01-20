# Recode values with no label to NA

For labelled variables, values with no label will be recoded to `NA`.

## Usage

``` r
nolabel_to_na(x)
```

## Arguments

- x:

  Object to recode.

## Examples

``` r
v <- labelled(c(1, 2, 9, 1, 9), c(yes = 1, no = 2))
nolabel_to_na(v)
#> <labelled<double>[5]>
#> [1]  1  2 NA  1 NA
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no
```
