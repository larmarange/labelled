# Sort value labels

Sort value labels according to values or to labels

## Usage

``` r
sort_val_labels(x, according_to = c("values", "labels"), decreasing = FALSE)
```

## Arguments

- x:

  A labelled vector or a data.frame

- according_to:

  According to values or to labels?

- decreasing:

  In decreasing order?

## Examples

``` r
v <- labelled(c(1, 2, 3), c(maybe = 2, yes = 1, no = 3))
v
#> <labelled<double>[3]>
#> [1] 1 2 3
#> 
#> Labels:
#>  value label
#>      2 maybe
#>      1   yes
#>      3    no
sort_val_labels(v)
#> <labelled<double>[3]>
#> [1] 1 2 3
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2 maybe
#>      3    no
sort_val_labels(v, decreasing = TRUE)
#> <labelled<double>[3]>
#> [1] 1 2 3
#> 
#> Labels:
#>  value label
#>      3    no
#>      2 maybe
#>      1   yes
sort_val_labels(v, "l")
#> <labelled<double>[3]>
#> [1] 1 2 3
#> 
#> Labels:
#>  value label
#>      2 maybe
#>      3    no
#>      1   yes
sort_val_labels(v, "l", TRUE)
#> <labelled<double>[3]>
#> [1] 1 2 3
#> 
#> Labels:
#>  value label
#>      1   yes
#>      3    no
#>      2 maybe
```
