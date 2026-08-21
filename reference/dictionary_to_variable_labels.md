# Convert a dictionary data frame into a list of variable / value labels

These helpers could be used to generate, from a data frame corresponding
to a variable dictionary, a list of variable or value labels that could
be passed to
[`set_variable_labels()`](https://larmarange.github.io/labelled/reference/var_label.md)
or to
[`set_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.md).

## Usage

``` r
dictionary_to_variable_labels(dictionary, names_from = 1, labels_from = 2)

dictionary_to_value_labels(
  dictionary,
  names_from = 1,
  values_from = 2,
  labels_from = 3,
  delim_entries = NULL,
  delim_value_label = NULL,
  data = NULL
)
```

## Arguments

- dictionary:

  A data frame or a tibble containing the definitions of the variable /
  value labels.

- names_from:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column containing the names of the variables.

- labels_from:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column containing the labels (unused if `delim_value_label` is
  provided).

- values_from:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Column containing the values or the pairs of values and labels (see
  examples).

- delim_entries:

  Optional string giving the delimiter between several pairs of value
  and labels if provided in a unique row. `NULL` if each pair is defined
  in a separate row (see examples).

- delim_value_label:

  Optional string giving the delimiter between the value and the label.

- data:

  A data frame or a tibble containing the raw data to identify the type
  (integer, numeric, character) of each variable.

## Note

For more complex cases, you may consider using
[`tidyr::separate_longer_delim()`](https://tidyr.tidyverse.org/reference/separate_longer_delim.html)
or
[`tidyr::separate_wider_regex()`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html)
before calling `dictionary_to_value_labels()`.

## Examples

``` r
dic <- dplyr::tibble(
  variable = c("mpg", "vs", "am", "cyl"),
  label = c("miles / gallon", "Engine", "Transmission", NA),
  values = c(NA, "0:V-Shaped,1:straight", "0:automatic,1:manual", NA)
)
dic
#> # A tibble: 4 × 3
#>   variable label          values               
#>   <chr>    <chr>          <chr>                
#> 1 mpg      miles / gallon NA                   
#> 2 vs       Engine         0:V-Shaped,1:straight
#> 3 am       Transmission   0:automatic,1:manual 
#> 4 cyl      NA             NA                   

l <- dic %>% dictionary_to_variable_labels()
l
#> $mpg
#> [1] "miles / gallon"
#> 
#> $vs
#> [1] "Engine"
#> 
#> $am
#> [1] "Transmission"
#> 
#> $cyl
#> [1] NA
#> 
mtcars %>%
  set_variable_labels(.labels = l) %>%
  look_for()
#>  pos variable label          col_type missing values
#>  1   mpg      miles / gallon dbl      0             
#>  2   cyl      —              dbl      0             
#>  3   disp     —              dbl      0             
#>  4   hp       —              dbl      0             
#>  5   drat     —              dbl      0             
#>  6   wt       —              dbl      0             
#>  7   qsec     —              dbl      0             
#>  8   vs       Engine         dbl      0             
#>  9   am       Transmission   dbl      0             
#>  10  gear     —              dbl      0             
#>  11  carb     —              dbl      0             

vl <- dic %>%
  dictionary_to_value_labels(
    values_from = values,
    delim_entries = ",",
    delim_value_label = ":",
    data = mtcars
  )
mtcars %>%
  set_value_labels(.labels = vl) %>%
  look_for()
#>  pos variable label col_type missing values       
#>  1   mpg      —     dbl      0                    
#>  2   cyl      —     dbl      0                    
#>  3   disp     —     dbl      0                    
#>  4   hp       —     dbl      0                    
#>  5   drat     —     dbl      0                    
#>  6   wt       —     dbl      0                    
#>  7   qsec     —     dbl      0                    
#>  8   vs       —     dbl+lbl  0       [0] V-Shaped 
#>                                      [1] straight 
#>  9   am       —     dbl+lbl  0       [0] automatic
#>                                      [1] manual   
#>  10  gear     —     dbl      0                    
#>  11  carb     —     dbl      0                    

dic2 <- dplyr::tibble(
  variable = c("am", "am", "vs", "vs"),
  labels = c("0:automatic", "1:manual", "0:V-shaped", "1:straight")
)
dic2 %>%
  dictionary_to_value_labels(
    delim_value_label = ":",
    data = mtcars
  )
#> $am
#> automatic    manual 
#>         0         1 
#> 
#> $vs
#> V-shaped straight 
#>        0        1 
#> 

dic3 <- dplyr::tibble(
  code = c(0, 1, 0, 1),
  label = c("automatic", "manual", "V-shaped", "straight"),
  var = c("am", "am", "vs", "vs")
)
dic3 %>%
  dictionary_to_value_labels(
    names_from = var,
    values_from = code,
    labels_from = label,
    data = mtcars
  )
#> $am
#> automatic    manual 
#>         0         1 
#> 
#> $vs
#> V-shaped straight 
#>        0        1 
#> 
```
