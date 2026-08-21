# Generate a data dictionary and search for variables with \`look_for()\`

## Showing a summary of a data frame

### Default printing of tibbles

It is a common need to easily get a description of all variables in a
data frame.

When a data frame is converted into a tibble (e.g. with
[`dplyr::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)),
it as a nice printing showing the first rows of the data frame as well
as the type of column.

``` r

library(dplyr)
```

``` r

iris %>% as_tibble()
```

    ## # A tibble: 150 × 5
    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ##           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
    ##  1          5.1         3.5          1.4         0.2 setosa 
    ##  2          4.9         3            1.4         0.2 setosa 
    ##  3          4.7         3.2          1.3         0.2 setosa 
    ##  4          4.6         3.1          1.5         0.2 setosa 
    ##  5          5           3.6          1.4         0.2 setosa 
    ##  6          5.4         3.9          1.7         0.4 setosa 
    ##  7          4.6         3.4          1.4         0.3 setosa 
    ##  8          5           3.4          1.5         0.2 setosa 
    ##  9          4.4         2.9          1.4         0.2 setosa 
    ## 10          4.9         3.1          1.5         0.1 setosa 
    ## # ℹ 140 more rows

However, when you have too many variables, all of them cannot be printed
and their are just listed.

``` r

data(fertility, package = "questionr")
women
```

    ## # A tibble: 2,000 × 17
    ##    id_woman id_household weight interview_date date_of_birth   age  residency
    ##       <dbl>        <dbl>  <dbl> <date>         <date>        <dbl> <hvn_lbll>
    ##  1      391          381  1.80  2012-05-05     1997-03-07       15          2
    ##  2     1643         1515  1.80  2012-01-23     1982-01-06       30          2
    ##  3       85           85  1.80  2012-01-21     1979-01-01       33          2
    ##  4      881          844  1.80  2012-01-06     1968-03-29       43          2
    ##  5     1981         1797  1.80  2012-05-11     1986-05-25       25          2
    ##  6     1072         1015  0.998 2012-02-20     1993-07-03       18          2
    ##  7     1978         1794  0.998 2012-02-23     1967-01-28       45          2
    ##  8     1607         1486  0.998 2012-02-20     1989-01-21       23          2
    ##  9      738          711  0.192 2012-03-09     1962-07-24       49          2
    ## 10     1656         1525  0.192 2012-03-15     1980-12-25       31          2
    ## # ℹ 1,990 more rows
    ## # ℹ 10 more variables: region <hvn_lbll>, instruction <hvn_lbll>,
    ## #   employed <hvn_lbl_>, matri <hvn_lbll>, religion <hvn_lbll>,
    ## #   newspaper <hvn_lbll>, radio <hvn_lbll>, tv <hvn_lbll>,
    ## #   ideal_nb_children <hvn_lbl_>, test <hvn_lbl_>

Note: in **R** console, value labels (if defined) are usually printed
but they do not appear in a R markdown document like this vignette.

### `dplyr::glimpse()`

The function
[`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html)
allows you to have a quick look at all the variables in a data frame.

``` r

glimpse(iris)
```

    ## Rows: 150
    ## Columns: 5
    ## $ Sepal.Length <dbl> 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.9, 5.4, 4.…
    ## $ Sepal.Width  <dbl> 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1, 3.7, 3.…
    ## $ Petal.Length <dbl> 1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, 1.4, 1.5, 1.5, 1.…
    ## $ Petal.Width  <dbl> 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, 0.2, 0.1, 0.2, 0.…
    ## $ Species      <fct> setosa, setosa, setosa, setosa, setosa, setosa, setosa, s…

``` r

glimpse(women)
```

    ## Rows: 2,000
    ## Columns: 17
    ## $ id_woman          <dbl> 391, 1643, 85, 881, 1981, 1072, 1978, 1607, 738, 165…
    ## $ id_household      <dbl> 381, 1515, 85, 844, 1797, 1015, 1794, 1486, 711, 152…
    ## $ weight            <dbl> 1.803150, 1.803150, 1.803150, 1.803150, 1.803150, 0.…
    ## $ interview_date    <date> 2012-05-05, 2012-01-23, 2012-01-21, 2012-01-06, 201…
    ## $ date_of_birth     <date> 1997-03-07, 1982-01-06, 1979-01-01, 1968-03-29, 198…
    ## $ age               <dbl> 15, 30, 33, 43, 25, 18, 45, 23, 49, 31, 26, 45, 25, …
    ## $ residency         <hvn_lbll> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ region            <hvn_lbll> 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2,…
    ## $ instruction       <hvn_lbll> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 2, 1, 0,…
    ## $ employed          <hvn_lbl_> 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ matri             <hvn_lbll> 0, 2, 2, 2, 1, 0, 1, 1, 2, 5, 2, 3, 0, 2, 1, 2,…
    ## $ religion          <hvn_lbll> 1, 3, 2, 3, 2, 2, 3, 1, 3, 3, 2, 3, 2, 2, 2, 2,…
    ## $ newspaper         <hvn_lbll> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
    ## $ radio             <hvn_lbll> 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0,…
    ## $ tv                <hvn_lbll> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,…
    ## $ ideal_nb_children <hvn_lbl_> 4, 4, 4, 4, 4, 5, 10, 5, 4, 5, 6, 10, 2, 6, 6, …
    ## $ test              <hvn_lbl_> 0, 9, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0,…

It will show you the first values of each variable as well as the type
of each variable. However, some important informations are not
displayed:

- variable labels, when defined;
- value labels for labelled vectors;
- the list of levels for factors;
- the range of values for numerical variables.

### `labelled::look_for()`

[`look_for()`](https://larmarange.github.io/labelled/reference/look_for.md)
provided by the `labelled` package will print in the console a data
dictionary of all variables, showing variable labels when available, the
type of variable and a list of values corresponding to:

- levels for factors;
- value labels for labelled vectors;
- the range of observed values in the vector otherwise (if
  `details = "full"`).

``` r

library(labelled)
look_for(iris)
```

    ##  pos variable     label col_type missing values    
    ##  1   Sepal.Length —     dbl      0                 
    ##  2   Sepal.Width  —     dbl      0                 
    ##  3   Petal.Length —     dbl      0                 
    ##  4   Petal.Width  —     dbl      0                 
    ##  5   Species      —     fct      0       setosa    
    ##                                          versicolor
    ##                                          virginica

``` r

look_for(women)
```

    ##  pos variable          label              col_type missing values            
    ##  1   id_woman          Woman Id           dbl      0                         
    ##  2   id_household      Household Id       dbl      0                         
    ##  3   weight            Sample weight      dbl      0                         
    ##  4   interview_date    Interview date     date     0                         
    ##  5   date_of_birth     Date of birth      date     0                         
    ##  6   age               Age at last anniv~ dbl      0                         
    ##  7   residency         Urban / rural res~ dbl+lbl  0       [1] urban         
    ##                                                            [2] rural         
    ##  8   region            Region             dbl+lbl  0       [1] North         
    ##                                                            [2] East          
    ##                                                            [3] South         
    ##                                                            [4] West          
    ##  9   instruction       Level of instruct~ dbl+lbl  0       [0] none          
    ##                                                            [1] primary       
    ##                                                            [2] secondary     
    ##                                                            [3] higher        
    ##  10  employed          Employed?          dbl+lbl  7       [0] no            
    ##                                                            [1] yes           
    ##                                                            [9] missing       
    ##  11  matri             Matrimonial status dbl+lbl  0       [0] single        
    ##                                                            [1] married       
    ##                                                            [2] living togeth~
    ##                                                            [3] windowed      
    ##                                                            [4] divorced      
    ##                                                            [5] separated     
    ##  12  religion          Religion           dbl+lbl  4       [1] Muslim        
    ##                                                            [2] Christian     
    ##                                                            [3] Protestant    
    ##                                                            [4] no religion   
    ##                                                            [5] other         
    ##  13  newspaper         Read newspaper?    dbl+lbl  0       [0] no            
    ##                                                            [1] yes           
    ##  14  radio             Listen to radio?   dbl+lbl  0       [0] no            
    ##                                                            [1] yes           
    ##  15  tv                Watch TV?          dbl+lbl  0       [0] no            
    ##                                                            [1] yes           
    ##  16  ideal_nb_children Ideal number of c~ dbl+lbl  0       [96] don't know   
    ##                                                            [99] missing      
    ##  17  test              Ever tested for H~ dbl+lbl  29      [0] no            
    ##                                                            [1] yes           
    ##                                                            [9] missing

Note that
[`lookfor()`](https://larmarange.github.io/labelled/reference/look_for.md)
and
[`generate_dictionary()`](https://larmarange.github.io/labelled/reference/look_for.md)
are synonyms of
[`look_for()`](https://larmarange.github.io/labelled/reference/look_for.md)
and works exactly in the same way.

If there is not enough space to print full labels in the console, they
will be truncated (truncation is indicated by a `~`).

To get a nicely formatted table from
[`look_for()`](https://larmarange.github.io/labelled/reference/look_for.md),
you could use
[`to_gt()`](https://larmarange.github.io/labelled/reference/look_for.md)
(the [gt](https://gt.rstudio.com) package is required).

``` r

women %>%
  look_for() %>%
  to_gt()
```

[TABLE]

## Searching variables by key

When a data frame has dozens or even hundreds of variables, it could
become difficult to find a specific variable. In such case, you can
provide an optional list of keywords, which can be simple character
strings or regular expression, to search for specific variables.

``` r

# Look for a single keyword.
look_for(iris, "petal")
```

    ##  pos variable     label col_type missing values
    ##  3   Petal.Length —     dbl      0             
    ##  4   Petal.Width  —     dbl      0

``` r

look_for(iris, "s")
```

    ##  pos variable     label col_type missing values    
    ##  1   Sepal.Length —     dbl      0                 
    ##  2   Sepal.Width  —     dbl      0                 
    ##  5   Species      —     fct      0       setosa    
    ##                                          versicolor
    ##                                          virginica

``` r

# Look for with a regular expression
look_for(iris, "petal|species")
```

    ##  pos variable     label col_type missing values    
    ##  3   Petal.Length —     dbl      0                 
    ##  4   Petal.Width  —     dbl      0                 
    ##  5   Species      —     fct      0       setosa    
    ##                                          versicolor
    ##                                          virginica

``` r

look_for(iris, "s$")
```

    ##  pos variable label col_type missing values    
    ##  5   Species  —     fct      0       setosa    
    ##                                      versicolor
    ##                                      virginica

``` r

# Look for with several keywords
look_for(iris, "pet", "sp")
```

    ##  pos variable     label col_type missing values    
    ##  3   Petal.Length —     dbl      0                 
    ##  4   Petal.Width  —     dbl      0                 
    ##  5   Species      —     fct      0       setosa    
    ##                                          versicolor
    ##                                          virginica

``` r

# Look_for will take variable labels into account
look_for(women, "read", "level")
```

    ##  pos variable    label                col_type missing values       
    ##  9   instruction Level of instruction dbl+lbl  0       [0] none     
    ##                                                        [1] primary  
    ##                                                        [2] secondary
    ##                                                        [3] higher   
    ##  13  newspaper   Read newspaper?      dbl+lbl  0       [0] no       
    ##                                                        [1] yes

By default,
[`look_for()`](https://larmarange.github.io/labelled/reference/look_for.md)
will look through both variable names and variables labels. Use
`labels = FALSE` to look only through variable names.

``` r

look_for(women, "read")
```

    ##  pos variable  label           col_type missing values 
    ##  13  newspaper Read newspaper? dbl+lbl  0       [0] no 
    ##                                                 [1] yes

``` r

look_for(women, "read", labels = FALSE)
```

    ## ! Nothing found. Sorry.

Similarly, the search is by default case insensitive. To make the search
case sensitive, use `ignore.case = FALSE`.

``` r

look_for(iris, "sepal")
```

    ##  pos variable     label col_type missing values
    ##  1   Sepal.Length —     dbl      0             
    ##  2   Sepal.Width  —     dbl      0

``` r

look_for(iris, "sepal", ignore.case = FALSE)
```

    ## ! Nothing found. Sorry.

## Level of details

If you just want to use the search feature of
[`look_for()`](https://larmarange.github.io/labelled/reference/look_for.md)
without computing the details of each variable, simply indicate
`details = "none"` or `details = FALSE`.

``` r

look_for(women, "id", details = "none")
```

    ##  pos variable          label                   
    ##   1  id_woman          Woman Id                
    ##   2  id_household      Household Id            
    ##   7  residency         Urban / rural residency 
    ##  16  ideal_nb_children Ideal number of children

If you want more details (but can be time consuming for big data
frames), indicate `details = "full"` or `details = TRUE`.

``` r

look_for(women, details = "full")
```

    ##  pos variable          label              col_type missing unique_values
    ##  1   id_woman          Woman Id           dbl      0       2000         
    ##  2   id_household      Household Id       dbl      0       1814         
    ##  3   weight            Sample weight      dbl      0       351          
    ##  4   interview_date    Interview date     date     0       165          
    ##  5   date_of_birth     Date of birth      date     0       1740         
    ##  6   age               Age at last anniv~ dbl      0       36           
    ##  7   residency         Urban / rural res~ dbl+lbl  0       2            
    ##                                                                         
    ##  8   region            Region             dbl+lbl  0       4            
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ##  9   instruction       Level of instruct~ dbl+lbl  0       4            
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ##  10  employed          Employed?          dbl+lbl  7       3            
    ##                                                                         
    ##                                                                         
    ##  11  matri             Matrimonial status dbl+lbl  0       6            
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ##  12  religion          Religion           dbl+lbl  4       6            
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ##  13  newspaper         Read newspaper?    dbl+lbl  0       2            
    ##                                                                         
    ##  14  radio             Listen to radio?   dbl+lbl  0       2            
    ##                                                                         
    ##  15  tv                Watch TV?          dbl+lbl  0       2            
    ##                                                                         
    ##  16  ideal_nb_children Ideal number of c~ dbl+lbl  0       18           
    ##                                                                         
    ##  17  test              Ever tested for H~ dbl+lbl  29      3            
    ##                                                                         
    ##                                                                         
    ##  values             na_values na_range
    ##  range: 1 - 2000                      
    ##  range: 1 - 1814                      
    ##  range: 0.044629 -~                   
    ##  range: 2011-12-01~                   
    ##  range: 1962-02-07~                   
    ##  range: 14 - 49                       
    ##  [1] urban                            
    ##  [2] rural                            
    ##  [1] North                            
    ##  [2] East                             
    ##  [3] South                            
    ##  [4] West                             
    ##  [0] none                             
    ##  [1] primary                          
    ##  [2] secondary                        
    ##  [3] higher                           
    ##  [0] no             9                 
    ##  [1] yes                              
    ##  [9] missing                          
    ##  [0] single                           
    ##  [1] married                          
    ##  [2] living togeth~                   
    ##  [3] windowed                         
    ##  [4] divorced                         
    ##  [5] separated                        
    ##  [1] Muslim                           
    ##  [2] Christian                        
    ##  [3] Protestant                       
    ##  [4] no religion                      
    ##  [5] other                            
    ##  [0] no                               
    ##  [1] yes                              
    ##  [0] no                               
    ##  [1] yes                              
    ##  [0] no                               
    ##  [1] yes                              
    ##  [96] don't know                      
    ##  [99] missing                         
    ##  [0] no             9                 
    ##  [1] yes                              
    ##  [9] missing

``` r

look_for(women, details = "full") %>%
  dplyr::glimpse()
```

    ## Rows: 17
    ## Columns: 14
    ## $ pos           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17
    ## $ variable      <chr> "id_woman", "id_household", "weight", "interview_date", …
    ## $ label         <chr> "Woman Id", "Household Id", "Sample weight", "Interview …
    ## $ col_type      <chr> "dbl", "dbl", "dbl", "date", "date", "dbl", "dbl+lbl", "…
    ## $ missing       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 4, 0, 0, 0, 0, 29
    ## $ levels        <named list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, …
    ## $ value_labels  <named list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <1, 2>, …
    ## $ class         <named list> "numeric", "numeric", "numeric", "Date", "Date", …
    ## $ type          <chr> "double", "double", "double", "double", "double",…
    ## $ na_values     <named list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <…
    ## $ na_range      <named list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, …
    ## $ n_na          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 4, 0, 0, 0, 0, 29
    ## $ unique_values <int> 2000, 1814, 351, 165, 1740, 36, 2, 4, 4, 3, 6, 6,…
    ## $ range         <named list> <1, 2000>, <1, 1814>, <0.044629, 4.396831>, <2011…

``` r

look_for(women, details = "full") %>% to_gt()
```

[TABLE]

## Advanced usages of `look_for()`

[`look_for()`](https://larmarange.github.io/labelled/reference/look_for.md)
returns a detailed tibble which is summarized before printing. To
deactivate default printing and see full results, simply use
[`dplyr::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
[`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) or
even [`utils::View()`](https://rdrr.io/r/utils/View.html).

``` r

look_for(women) %>% View()
```

``` r

look_for(women) %>% as_tibble()
```

    ## # A tibble: 17 × 7
    ##      pos variable          label            col_type missing levels value_labels
    ##    <int> <chr>             <chr>            <chr>      <int> <name> <named list>
    ##  1     1 id_woman          Woman Id         dbl            0 <NULL> <NULL>      
    ##  2     2 id_household      Household Id     dbl            0 <NULL> <NULL>      
    ##  3     3 weight            Sample weight    dbl            0 <NULL> <NULL>      
    ##  4     4 interview_date    Interview date   date           0 <NULL> <NULL>      
    ##  5     5 date_of_birth     Date of birth    date           0 <NULL> <NULL>      
    ##  6     6 age               Age at last ann… dbl            0 <NULL> <NULL>      
    ##  7     7 residency         Urban / rural r… dbl+lbl        0 <NULL> <dbl [2]>   
    ##  8     8 region            Region           dbl+lbl        0 <NULL> <dbl [4]>   
    ##  9     9 instruction       Level of instru… dbl+lbl        0 <NULL> <dbl [4]>   
    ## 10    10 employed          Employed?        dbl+lbl        7 <NULL> <dbl [3]>   
    ## 11    11 matri             Matrimonial sta… dbl+lbl        0 <NULL> <dbl [6]>   
    ## 12    12 religion          Religion         dbl+lbl        4 <NULL> <dbl [5]>   
    ## 13    13 newspaper         Read newspaper?  dbl+lbl        0 <NULL> <dbl [2]>   
    ## 14    14 radio             Listen to radio? dbl+lbl        0 <NULL> <dbl [2]>   
    ## 15    15 tv                Watch TV?        dbl+lbl        0 <NULL> <dbl [2]>   
    ## 16    16 ideal_nb_children Ideal number of… dbl+lbl        0 <NULL> <dbl [2]>   
    ## 17    17 test              Ever tested for… dbl+lbl       29 <NULL> <dbl [3]>

``` r

glimpse(look_for(women))
```

    ## Rows: 17
    ## Columns: 7
    ## $ pos          <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17
    ## $ variable     <chr> "id_woman", "id_household", "weight", "interview_date", "…
    ## $ label        <chr> "Woman Id", "Household Id", "Sample weight", "Interview d…
    ## $ col_type     <chr> "dbl", "dbl", "dbl", "date", "date", "dbl", "dbl+lbl", "d…
    ## $ missing      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 4, 0, 0, 0, 0, 29
    ## $ levels       <named list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <…
    ## $ value_labels <named list> <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <NULL>, <1, 2>, <…

The tibble returned by
[`look_for()`](https://larmarange.github.io/labelled/reference/look_for.md)
could be easily manipulated for advanced programming.

When a column has several values for one variable (e.g. `levels` or
`value_labels`), results as stored with nested named list. You can
convert named lists into simpler character vectors, you can use
[`convert_list_columns_to_character()`](https://larmarange.github.io/labelled/reference/look_for.md).

``` r

look_for(women) %>% convert_list_columns_to_character()
```

    ## # A tibble: 17 × 7
    ##      pos variable          label            col_type missing levels value_labels
    ##    <int> <chr>             <chr>            <chr>      <int> <chr>  <chr>       
    ##  1     1 id_woman          Woman Id         dbl            0 ""     ""          
    ##  2     2 id_household      Household Id     dbl            0 ""     ""          
    ##  3     3 weight            Sample weight    dbl            0 ""     ""          
    ##  4     4 interview_date    Interview date   date           0 ""     ""          
    ##  5     5 date_of_birth     Date of birth    date           0 ""     ""          
    ##  6     6 age               Age at last ann… dbl            0 ""     ""          
    ##  7     7 residency         Urban / rural r… dbl+lbl        0 ""     "[1] urban;…
    ##  8     8 region            Region           dbl+lbl        0 ""     "[1] North;…
    ##  9     9 instruction       Level of instru… dbl+lbl        0 ""     "[0] none; …
    ## 10    10 employed          Employed?        dbl+lbl        7 ""     "[0] no; [1…
    ## 11    11 matri             Matrimonial sta… dbl+lbl        0 ""     "[0] single…
    ## 12    12 religion          Religion         dbl+lbl        4 ""     "[1] Muslim…
    ## 13    13 newspaper         Read newspaper?  dbl+lbl        0 ""     "[0] no; [1…
    ## 14    14 radio             Listen to radio? dbl+lbl        0 ""     "[0] no; [1…
    ## 15    15 tv                Watch TV?        dbl+lbl        0 ""     "[0] no; [1…
    ## 16    16 ideal_nb_children Ideal number of… dbl+lbl        0 ""     "[96] don't…
    ## 17    17 test              Ever tested for… dbl+lbl       29 ""     "[0] no; [1…

Alternatively, you can use
[`lookfor_to_long_format()`](https://larmarange.github.io/labelled/reference/look_for.md)
to transform results into a long format with one row per factor level
and per value label.

``` r

look_for(women) %>% lookfor_to_long_format()
```

    ## # A tibble: 41 × 7
    ##      pos variable       label               col_type missing levels value_labels
    ##    <int> <chr>          <chr>               <chr>      <int> <chr>  <chr>       
    ##  1     1 id_woman       Woman Id            dbl            0 NA     NA          
    ##  2     2 id_household   Household Id        dbl            0 NA     NA          
    ##  3     3 weight         Sample weight       dbl            0 NA     NA          
    ##  4     4 interview_date Interview date      date           0 NA     NA          
    ##  5     5 date_of_birth  Date of birth       date           0 NA     NA          
    ##  6     6 age            Age at last annive… dbl            0 NA     NA          
    ##  7     7 residency      Urban / rural resi… dbl+lbl        0 NA     [1] urban   
    ##  8     7 residency      Urban / rural resi… dbl+lbl        0 NA     [2] rural   
    ##  9     8 region         Region              dbl+lbl        0 NA     [1] North   
    ## 10     8 region         Region              dbl+lbl        0 NA     [2] East    
    ## # ℹ 31 more rows

Both can be combined:

``` r

look_for(women) %>%
  lookfor_to_long_format() %>%
  convert_list_columns_to_character()
```

    ## # A tibble: 41 × 7
    ##      pos variable       label               col_type missing levels value_labels
    ##    <int> <chr>          <chr>               <chr>      <int> <chr>  <chr>       
    ##  1     1 id_woman       Woman Id            dbl            0 NA     NA          
    ##  2     2 id_household   Household Id        dbl            0 NA     NA          
    ##  3     3 weight         Sample weight       dbl            0 NA     NA          
    ##  4     4 interview_date Interview date      date           0 NA     NA          
    ##  5     5 date_of_birth  Date of birth       date           0 NA     NA          
    ##  6     6 age            Age at last annive… dbl            0 NA     NA          
    ##  7     7 residency      Urban / rural resi… dbl+lbl        0 NA     [1] urban   
    ##  8     7 residency      Urban / rural resi… dbl+lbl        0 NA     [2] rural   
    ##  9     8 region         Region              dbl+lbl        0 NA     [1] North   
    ## 10     8 region         Region              dbl+lbl        0 NA     [2] East    
    ## # ℹ 31 more rows
