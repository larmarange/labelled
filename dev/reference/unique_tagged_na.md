# Unique elements, duplicated, ordering and sorting with tagged NAs

These adaptations of
[`base::unique()`](https://rdrr.io/r/base/unique.html),
[`base::duplicated()`](https://rdrr.io/r/base/duplicated.html),
[`base::order()`](https://rdrr.io/r/base/order.html) and
[`base::sort()`](https://rdrr.io/r/base/sort.html) treats tagged NAs as
distinct values.

## Usage

``` r
unique_tagged_na(x, fromLast = FALSE)

duplicated_tagged_na(x, fromLast = FALSE)

order_tagged_na(
  x,
  na.last = TRUE,
  decreasing = FALSE,
  method = c("auto", "shell", "radix"),
  na_decreasing = decreasing,
  untagged_na_last = TRUE
)

sort_tagged_na(
  x,
  decreasing = FALSE,
  na.last = TRUE,
  na_decreasing = decreasing,
  untagged_na_last = TRUE
)
```

## Arguments

- x:

  a vector

- fromLast:

  logical indicating if duplication should be considered from the last

- na.last:

  if `TRUE`, missing values in the data are put last; if `FALSE`, they
  are put first

- decreasing:

  should the sort order be increasing or decreasing?

- method:

  the method to be used, see
  [`base::order()`](https://rdrr.io/r/base/order.html)

- na_decreasing:

  should the sort order for tagged NAs value be

- untagged_na_last:

  should untagged `NA`s be sorted after tagged `NA`s? increasing or
  decreasing?

## Examples

``` r
x <- c(1, 2, tagged_na("a"), 1, tagged_na("z"), 2, tagged_na("a"), NA)
x %>% print_tagged_na()
#> [1]     1     2 NA(a)     1 NA(z)     2 NA(a)    NA

unique(x) %>% print_tagged_na()
#> [1]     1     2 NA(a)
unique_tagged_na(x) %>% print_tagged_na()
#> [1]     1     2 NA(a) NA(z)    NA

duplicated(x)
#> [1] FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
duplicated_tagged_na(x)
#> [1] FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE

order(x)
#> [1] 1 4 2 6 3 5 7 8
order_tagged_na(x)
#> [1] 1 4 2 6 3 7 5 8

sort(x, na.last = TRUE) %>% print_tagged_na()
#> [1]     1     1     2     2 NA(a) NA(z) NA(a)    NA
sort_tagged_na(x) %>% print_tagged_na()
#> [1]     1     1     2     2 NA(a) NA(a) NA(z)    NA
```
