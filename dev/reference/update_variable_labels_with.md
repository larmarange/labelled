# Update variable/value labels with a function

Update variable/value labels with a function

## Usage

``` r
update_variable_labels_with(.data, .fn, .cols = dplyr::everything(), ...)

update_value_labels_with(.data, .fn, .cols = dplyr::everything(), ...)
```

## Arguments

- .data:

  A data frame, a data frame extension (e.g. a tibble) or a survey
  design

- .fn:

  A function used to transform the variable/value labels of the selected
  `.cols`.

- .cols:

  Columns to update; defaults to all columns. Use tidy selection.

- ...:

  additional arguments passed onto `.fn`.

## Details

For `update_variable_labels_with()`, it is possible to access the name
of the variable inside `.fn` by using
[`names()`](https://rdrr.io/r/base/names.html), i.e. `.fn` receive a
named character vector (see example). `.fn` can return
`as.character(NA)` to remove a variable label.

## Examples

``` r
df <- iris %>%
  set_variable_labels(
    Sepal.Length = "Length of sepal",
    Sepal.Width = "Width of sepal",
    Petal.Length = "Length of petal",
    Petal.Width = "Width of petal",
    Species = "Species"
  )
df$Species <- to_labelled(df$Species)
df %>% look_for()
#>  pos variable     label           col_type missing values        
#>  1   Sepal.Length Length of sepal dbl      0                     
#>  2   Sepal.Width  Width of sepal  dbl      0                     
#>  3   Petal.Length Length of petal dbl      0                     
#>  4   Petal.Width  Width of petal  dbl      0                     
#>  5   Species      Species         dbl+lbl  0       [1] setosa    
#>                                                    [2] versicolor
#>                                                    [3] virginica 
df %>%
  update_variable_labels_with(toupper) %>%
  look_for()
#>  pos variable     label           col_type missing values        
#>  1   Sepal.Length LENGTH OF SEPAL dbl      0                     
#>  2   Sepal.Width  WIDTH OF SEPAL  dbl      0                     
#>  3   Petal.Length LENGTH OF PETAL dbl      0                     
#>  4   Petal.Width  WIDTH OF PETAL  dbl      0                     
#>  5   Species      SPECIES         dbl+lbl  0       [1] setosa    
#>                                                    [2] versicolor
#>                                                    [3] virginica 

# accessing variable names with names()
df %>%
  update_variable_labels_with(function(x){tolower(names(x))}) %>%
  look_for()
#>  pos variable     label        col_type missing values        
#>  1   Sepal.Length sepal.length dbl      0                     
#>  2   Sepal.Width  sepal.width  dbl      0                     
#>  3   Petal.Length petal.length dbl      0                     
#>  4   Petal.Width  petal.width  dbl      0                     
#>  5   Species      species      dbl+lbl  0       [1] setosa    
#>                                                 [2] versicolor
#>                                                 [3] virginica 

df %>%
  update_variable_labels_with(toupper, .cols = dplyr::starts_with("S")) %>%
  look_for()
#>  pos variable     label           col_type missing values        
#>  1   Sepal.Length LENGTH OF SEPAL dbl      0                     
#>  2   Sepal.Width  WIDTH OF SEPAL  dbl      0                     
#>  3   Petal.Length Length of petal dbl      0                     
#>  4   Petal.Width  Width of petal  dbl      0                     
#>  5   Species      SPECIES         dbl+lbl  0       [1] setosa    
#>                                                    [2] versicolor
#>                                                    [3] virginica 
df %>%
  update_value_labels_with(toupper) %>%
  look_for()
#>  pos variable     label           col_type missing values        
#>  1   Sepal.Length Length of sepal dbl      0                     
#>  2   Sepal.Width  Width of sepal  dbl      0                     
#>  3   Petal.Length Length of petal dbl      0                     
#>  4   Petal.Width  Width of petal  dbl      0                     
#>  5   Species      Species         dbl+lbl  0       [1] SETOSA    
#>                                                    [2] VERSICOLOR
#>                                                    [3] VIRGINICA 
```
