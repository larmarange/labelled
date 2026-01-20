# Recode some values based on condition

Recode some values based on condition

## Usage

``` r
recode_if(x, condition, true)
```

## Arguments

- x:

  vector to be recoded

- condition:

  logical vector of same length as `x`

- true:

  values to use for `TRUE` values of `condition`. It must be either the
  same length as `x`, or length 1.

## Value

Returns `x` with values replaced by `true` when `condition` is `TRUE`
and unchanged when `condition` is `FALSE` or `NA`. Variable and value
labels are preserved unchanged.

## Examples

``` r
v <- labelled(c(1, 2, 2, 9), c(yes = 1, no = 2))
v %>% recode_if(v == 9, NA)
#> <labelled<double>[4]>
#> [1]  1  2  2 NA
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no
if (require(dplyr)) {
  df <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 2, 1)) %>%
    set_value_labels(
      s1 = c(Male = "M", Female = "F"),
      s2 = c(A = 1, B = 2)
    ) %>%
    set_variable_labels(s1 = "Gender", s2 = "Group")

  df <- df %>%
    mutate(
      s3 = s2 %>% recode_if(s1 == "F", 2),
      s4 = s2 %>% recode_if(s1 == "M", s2 + 10)
    )
  df
  df %>% look_for()
}
#>  pos variable label  col_type missing values    
#>  1   s1       Gender chr+lbl  0       [M] Male  
#>                                       [F] Female
#>  2   s2       Group  dbl+lbl  0       [1] A     
#>                                       [2] B     
#>  3   s3       Group  dbl+lbl  0       [1] A     
#>                                       [2] B     
#>  4   s4       Group  dbl+lbl  0       [1] A     
#>                                       [2] B     
```
