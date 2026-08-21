# Drop unused value labels

Drop value labels associated to a value not present in the data.

## Usage

``` r
drop_unused_value_labels(x)
```

## Arguments

- x:

  A vector, a data frame or a survey design.

## Examples

``` r
x <- labelled(c(1, 2, 2, 1), c(yes = 1, no = 2, maybe = 3))
x
#> <labelled<double>[4]>
#> [1] 1 2 2 1
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no
#>      3 maybe
drop_unused_value_labels(x)
#> <labelled<double>[4]>
#> [1] 1 2 2 1
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no
```
