# Turn a named vector into a vector of names prefixed by values

Turn a named vector into a vector of names prefixed by values

## Usage

``` r
names_prefixed_by_values(x, sep = "[]")
```

## Arguments

- x:

  vector to be prefixed

- sep:

  (string) separator between value and name

## Examples

``` r
df <- dplyr::tibble(
  c1 = labelled(c("M", "M", "F"), c(Male = "M", Female = "F")),
  c2 = labelled(c(1, 1, 2), c(Yes = 1, No = 2))
)
val_labels(df$c1)
#>   Male Female 
#>    "M"    "F" 
val_labels(df$c1) %>% names_prefixed_by_values()
#>         Male       Female 
#>   "[M] Male" "[F] Female" 
val_labels(df)
#> $c1
#>   Male Female 
#>    "M"    "F" 
#> 
#> $c2
#> Yes  No 
#>   1   2 
#> 
val_labels(df) %>% names_prefixed_by_values()
#> $c1
#>         Male       Female 
#>   "[M] Male" "[F] Female" 
#> 
#> $c2
#>       Yes        No 
#> "[1] Yes"  "[2] No" 
#> 
val_labels(df) %>% names_prefixed_by_values(sep = ":")
#> $c1
#>       Male     Female 
#>   "M:Male" "F:Female" 
#> 
#> $c2
#>     Yes      No 
#> "1:Yes"  "2:No" 
#> 
```
