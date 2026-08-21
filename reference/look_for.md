# Look for keywords variable names and descriptions / Create a data dictionary

`look_for()` emulates the `lookfor` Stata command in R. It supports
searching into the variable names of regular R data frames as well as
into variable labels descriptions, factor levels and value labels. The
command is meant to help users finding variables in large datasets.

## Usage

``` r
look_for(
  data,
  ...,
  labels = TRUE,
  values = TRUE,
  ignore.case = TRUE,
  details = c("basic", "none", "full")
)

lookfor(
  data,
  ...,
  labels = TRUE,
  values = TRUE,
  ignore.case = TRUE,
  details = c("basic", "none", "full")
)

generate_dictionary(
  data,
  ...,
  labels = TRUE,
  values = TRUE,
  ignore.case = TRUE,
  details = c("basic", "none", "full")
)

# S3 method for class 'look_for'
print(x, ...)

look_for_and_select(
  data,
  ...,
  labels = TRUE,
  values = TRUE,
  ignore.case = TRUE
)

convert_list_columns_to_character(x, sep_value_labels = "[]", sep_other = "; ")

lookfor_to_long_format(x, sep_value_labels = "[]")

to_gt(
  x,
  column_labels = list(pos = "#", variable = "Variable", col_type = "Type", label =
    "Variable label", values = "Values", missing = "Missing values", unique_values =
    "Unique values", na_values = "User-defined missings (values)", na_range =
    "User-defined missings (range)")
)
```

## Source

Inspired by the `lookfor` command in Stata.

## Arguments

- data:

  a data frame or a survey object

- ...:

  optional list of keywords, a character string (or several character
  strings), which can be formatted as a regular expression suitable for
  a [`base::grep()`](https://rdrr.io/r/base/grep.html) pattern, or a
  vector of keywords; displays all variables if not specified

- labels:

  whether or not to search variable labels (descriptions); `TRUE` by
  default

- values:

  whether or not to search within values (factor levels or value
  labels); `TRUE` by default

- ignore.case:

  whether or not to make the keywords case sensitive; `TRUE` by default
  (case is ignored during matching)

- details:

  add details about each variable (full details could be time consuming
  for big data frames, `FALSE` is equivalent to `"none"` and `TRUE` to
  `"full"`)

- x:

  a tibble returned by `look_for()`

- sep_value_labels:

  (string) for value labels, separator between value and name

- sep_other:

  separator for other list columns

- column_labels:

  Optional column labels

## Value

a tibble data frame featuring the variable position, name and
description (if it exists) in the original data frame

## Details

When no keyword is provided, it will produce a data dictionary of the
overall data frame.

The function looks into the variable names for matches to the keywords.
If available, variable labels are included in the search scope. Variable
labels of data.frame imported with foreign or memisc packages will also
be taken into account (see
[`to_labelled()`](https://larmarange.github.io/labelled/reference/to_labelled.md)).
If no keyword is provided, it will return all variables of `data`.

`look_for()`, `lookfor()` and `generate_dictionary()` are equivalent.

By default, results will be summarized when printing. To deactivate
default printing, use
[`dplyr::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).

`lookfor_to_long_format()` could be used to transform results with one
row per factor level and per value label.

Use `convert_list_columns_to_character()` to convert named list columns
into character vectors (see examples).

The function `to_gt()` transform the results of `look_for()` into a
nicely formatted table using
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html). This table could
be easily be exported to a file with
[`gt::gtsave()`](https://gt.rstudio.com/reference/gtsave.html).
[`gt::tab_header()`](https://gt.rstudio.com/reference/tab_header.html)
could be used to add a title to the table (see examples).

`look_for_and_select()` is a shortcut for selecting some variables and
applying
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)s
to return a data frame with only the selected variables.

## See also

[`vignette("look_for")`](https://larmarange.github.io/labelled/articles/look_for.md)

## Author

François Briatte <f.briatte@gmail.com>, Joseph Larmarange
<joseph@larmarange.net>

## Examples

``` r
look_for(iris)
#>  pos variable     label col_type missing values    
#>  1   Sepal.Length —     dbl      0                 
#>  2   Sepal.Width  —     dbl      0                 
#>  3   Petal.Length —     dbl      0                 
#>  4   Petal.Width  —     dbl      0                 
#>  5   Species      —     fct      0       setosa    
#>                                          versicolor
#>                                          virginica 

# Look for a single keyword.
look_for(iris, "petal")
#>  pos variable     label col_type missing values
#>  3   Petal.Length —     dbl      0             
#>  4   Petal.Width  —     dbl      0             
look_for(iris, "s")
#>  pos variable     label col_type missing values    
#>  1   Sepal.Length —     dbl      0                 
#>  2   Sepal.Width  —     dbl      0                 
#>  5   Species      —     fct      0       setosa    
#>                                          versicolor
#>                                          virginica 
iris %>%
  look_for_and_select("s") %>%
  head()
#>   Sepal.Length Sepal.Width Species
#> 1          5.1         3.5  setosa
#> 2          4.9         3.0  setosa
#> 3          4.7         3.2  setosa
#> 4          4.6         3.1  setosa
#> 5          5.0         3.6  setosa
#> 6          5.4         3.9  setosa

# Look for with a regular expression
look_for(iris, "petal|species")
#>  pos variable     label col_type missing values    
#>  3   Petal.Length —     dbl      0                 
#>  4   Petal.Width  —     dbl      0                 
#>  5   Species      —     fct      0       setosa    
#>                                          versicolor
#>                                          virginica 
look_for(iris, "s$")
#>  pos variable label col_type missing values    
#>  5   Species  —     fct      0       setosa    
#>                                      versicolor
#>                                      virginica 

# Look for with several keywords
look_for(iris, "pet", "sp")
#>  pos variable     label col_type missing values    
#>  3   Petal.Length —     dbl      0                 
#>  4   Petal.Width  —     dbl      0                 
#>  5   Species      —     fct      0       setosa    
#>                                          versicolor
#>                                          virginica 
look_for(iris, "pet", "sp", "width")
#>  pos variable     label col_type missing values    
#>  2   Sepal.Width  —     dbl      0                 
#>  3   Petal.Length —     dbl      0                 
#>  4   Petal.Width  —     dbl      0                 
#>  5   Species      —     fct      0       setosa    
#>                                          versicolor
#>                                          virginica 
look_for(iris, "Pet", "sp", "width", ignore.case = FALSE)
#>  pos variable     label col_type missing values
#>  3   Petal.Length —     dbl      0             
#>  4   Petal.Width  —     dbl      0             

# Look_for can search within factor levels or value labels
look_for(iris, "vers")
#>  pos variable label col_type missing values    
#>  5   Species  —     fct      0       setosa    
#>                                      versicolor
#>                                      virginica 

# Quicker search without variable details
look_for(iris, details = "none")
#>  pos variable     label
#>  1   Sepal.Length —    
#>  2   Sepal.Width  —    
#>  3   Petal.Length —    
#>  4   Petal.Width  —    
#>  5   Species      —    

# To obtain more details about each variable
look_for(iris, details = "full")
#>  pos variable     label col_type missing unique_values values          
#>  1   Sepal.Length —     dbl      0       35            range: 4.3 - 7.9
#>  2   Sepal.Width  —     dbl      0       23            range: 2 - 4.4  
#>  3   Petal.Length —     dbl      0       43            range: 1 - 6.9  
#>  4   Petal.Width  —     dbl      0       22            range: 0.1 - 2.5
#>  5   Species      —     fct      0       3             setosa          
#>                                                        versicolor      
#>                                                        virginica       
#>  na_values na_range
#>                    
#>                    
#>                    
#>                    
#>                    
#>                    
#>                    

# To deactivate default printing, convert to tibble
look_for(iris, details = "full") %>%
  dplyr::as_tibble()
#> # A tibble: 5 × 14
#>     pos variable     label col_type missing levels      value_labels class type 
#>   <int> <chr>        <chr> <chr>      <int> <named lis> <named list> <nam> <chr>
#> 1     1 Sepal.Length NA    dbl            0 <NULL>      <NULL>       <chr> doub…
#> 2     2 Sepal.Width  NA    dbl            0 <NULL>      <NULL>       <chr> doub…
#> 3     3 Petal.Length NA    dbl            0 <NULL>      <NULL>       <chr> doub…
#> 4     4 Petal.Width  NA    dbl            0 <NULL>      <NULL>       <chr> doub…
#> 5     5 Species      NA    fct            0 <chr [3]>   <NULL>       <chr> inte…
#> # ℹ 5 more variables: na_values <named list>, na_range <named list>,
#> #   n_na <int>, unique_values <int>, range <named list>

# To convert named lists into character vectors
look_for(iris) %>% convert_list_columns_to_character()
#> # A tibble: 5 × 7
#>     pos variable     label col_type missing levels                  value_labels
#>   <int> <chr>        <chr> <chr>      <int> <chr>                   <chr>       
#> 1     1 Sepal.Length NA    dbl            0 ""                      ""          
#> 2     2 Sepal.Width  NA    dbl            0 ""                      ""          
#> 3     3 Petal.Length NA    dbl            0 ""                      ""          
#> 4     4 Petal.Width  NA    dbl            0 ""                      ""          
#> 5     5 Species      NA    fct            0 "setosa; versicolor; v… ""          

# Long format with one row per factor and per value label
look_for(iris) %>% lookfor_to_long_format()
#> # A tibble: 7 × 7
#>     pos variable     label col_type missing levels     value_labels
#>   <int> <chr>        <chr> <chr>      <int> <chr>      <chr>       
#> 1     1 Sepal.Length NA    dbl            0 NA         NA          
#> 2     2 Sepal.Width  NA    dbl            0 NA         NA          
#> 3     3 Petal.Length NA    dbl            0 NA         NA          
#> 4     4 Petal.Width  NA    dbl            0 NA         NA          
#> 5     5 Species      NA    fct            0 setosa     NA          
#> 6     5 Species      NA    fct            0 versicolor NA          
#> 7     5 Species      NA    fct            0 virginica  NA          

# Both functions can be combined
look_for(iris) %>%
  lookfor_to_long_format() %>%
  convert_list_columns_to_character()
#> # A tibble: 7 × 7
#>     pos variable     label col_type missing levels     value_labels
#>   <int> <chr>        <chr> <chr>      <int> <chr>      <chr>       
#> 1     1 Sepal.Length NA    dbl            0 NA         NA          
#> 2     2 Sepal.Width  NA    dbl            0 NA         NA          
#> 3     3 Petal.Length NA    dbl            0 NA         NA          
#> 4     4 Petal.Width  NA    dbl            0 NA         NA          
#> 5     5 Species      NA    fct            0 setosa     NA          
#> 6     5 Species      NA    fct            0 versicolor NA          
#> 7     5 Species      NA    fct            0 virginica  NA          

# Labelled data
d <- dplyr::tibble(
  region = labelled_spss(
    c(1, 2, 1, 9, 2, 3),
    c(north = 1, south = 2, center = 3, missing = 9),
    na_values = 9,
    label = "Region of the respondent"
  ),
  sex = labelled(
    c("f", "f", "m", "m", "m", "f"),
    c(female = "f", male = "m"),
    label = "Sex of the respondent"
  )
)
look_for(d)
#>  pos variable label                    col_type missing values     
#>  1   region   Region of the respondent dbl+lbl  1       [1] north  
#>                                                         [2] south  
#>                                                         [3] center 
#>                                                         [9] missing
#>  2   sex      Sex of the respondent    chr+lbl  0       [f] female 
#>                                                         [m] male   
d %>%
  look_for() %>%
  lookfor_to_long_format(sep_value_labels = ":")
#> # A tibble: 6 × 7
#>     pos variable label                    col_type missing levels value_labels
#>   <int> <chr>    <chr>                    <chr>      <int> <chr>  <chr>       
#> 1     1 region   Region of the respondent dbl+lbl        1 NA     1:north     
#> 2     1 region   Region of the respondent dbl+lbl        1 NA     2:south     
#> 3     1 region   Region of the respondent dbl+lbl        1 NA     3:center    
#> 4     1 region   Region of the respondent dbl+lbl        1 NA     9:missing   
#> 5     2 sex      Sex of the respondent    chr+lbl        0 NA     f:female    
#> 6     2 sex      Sex of the respondent    chr+lbl        0 NA     m:male      
d %>%
  look_for() %>%
  convert_list_columns_to_character(sep_value_labels = ":", sep_other = "|")
#> # A tibble: 2 × 7
#>     pos variable label                    col_type missing levels value_labels  
#>   <int> <chr>    <chr>                    <chr>      <int> <chr>  <chr>         
#> 1     1 region   Region of the respondent dbl+lbl        1 ""     1:north|2:sou…
#> 2     2 sex      Sex of the respondent    chr+lbl        0 ""     f:female|m:ma…
iris %>% look_for(details = TRUE) %>% to_gt()


  

#
```
