# Convert tagged NAs into user NAs

`tagged_na_to_user_na()` is the opposite of
[`user_na_to_tagged_na()`](https://larmarange.github.io/labelled/reference/na_values.md)
and convert tagged `NA` into user defined missing values (see
[`labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)).

## Usage

``` r
tagged_na_to_user_na(x, user_na_start = NULL)

tagged_na_to_regular_na(x)
```

## Arguments

- x:

  a vector, a data frame or a survey design

- user_na_start:

  minimum value of the new user na, if `NULL`, computed automatically
  (maximum of observed values + 1)

## Details

`tagged_na_to_regular_na()` converts tagged NAs into regular NAs.

## Examples

``` r
x <- c(1:5, tagged_na("a"), tagged_na("z"), NA)
x
#> [1]  1  2  3  4  5 NA NA NA
print_tagged_na(x)
#> [1]     1     2     3     4     5 NA(a) NA(z)    NA
tagged_na_to_user_na(x)
#> <labelled_spss<double>[8]>
#> [1]  1  2  3  4  5  6  7 NA
#> Missing range:  [6, 7]
#> 
#> Labels:
#>  value label
#>      6 NA(a)
#>      7 NA(z)
tagged_na_to_user_na(x, user_na_start = 10)
#> <labelled_spss<double>[8]>
#> [1]  1  2  3  4  5 10 11 NA
#> Missing range:  [10, 11]
#> 
#> Labels:
#>  value label
#>     10 NA(a)
#>     11 NA(z)

y <- c(1, 0, 1, tagged_na("r"), 0, tagged_na("d"))
val_labels(y) <- c(
  no = 0, yes = 1,
  "don't know" = tagged_na("d"),
  refusal = tagged_na("r")
)
y
#> <labelled<double>[6]>
#> [1]     1     0     1 NA(r)     0 NA(d)
#> 
#> Labels:
#>  value      label
#>      0         no
#>      1        yes
#>  NA(d) don't know
#>  NA(r)    refusal
tagged_na_to_user_na(y, user_na_start = 8)
#> <labelled_spss<double>[6]>
#> [1] 1 0 1 9 0 8
#> Missing range:  [8, 9]
#> 
#> Labels:
#>  value      label
#>      0         no
#>      1        yes
#>      8 don't know
#>      9    refusal
tagged_na_to_regular_na(y)
#> <labelled<double>[6]>
#> [1]  1  0  1 NA  0 NA
#> 
#> Labels:
#>  value label
#>      0    no
#>      1   yes
tagged_na_to_regular_na(y) %>% is_tagged_na()
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE
```
