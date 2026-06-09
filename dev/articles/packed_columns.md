# Variables labels and packed columns

The **tidyr** package allows to group several columns of a tibble into
one single df-column, see
[`tidyr::pack()`](https://tidyr.tidyverse.org/reference/pack.html). Such
df-column is itself a tibble. It’s not currently clear why you would
ever want to pack columns since few functions work with this sort of
data.

``` r

library(tidyr)
d <- iris %>%
  as_tibble() %>%
  pack(
    Sepal = starts_with("Sepal"),
    Petal = starts_with("Petal"),
    .names_sep = "."
  )
str(d)
```

    ## tibble [150 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Species: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Sepal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##   ..$ Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##  $ Petal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##   ..$ Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...

``` r

class(d$Sepal)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

Regarding variable labels, you may want to define a label for one
sub-column of a df-column, or eventually a label for the df-column
itself.

For a sub-column, you could use easily
[`var_label()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
to define your label.

``` r

library(labelled)
var_label(d$Sepal$Length) <- "Length of the sepal"
str(d)
```

    ## tibble [150 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Species: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Sepal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##   .. ..- attr(*, "label")= chr "Length of the sepal"
    ##   ..$ Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##  $ Petal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##   ..$ Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...

But you cannot use directly
[`var_label()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
for the df-column.

``` r

var_label(d$Petal) <- "wrong label for Petal"
str(d)
```

    ## tibble [150 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Species: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Sepal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##   .. ..- attr(*, "label")= chr "Length of the sepal"
    ##   ..$ Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##  $ Petal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##   .. ..- attr(*, "label")= chr "wrong label for Petal"
    ##   ..$ Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##   .. ..- attr(*, "label")= chr "wrong label for Petal"

As `d$Petal` is itself a tibble, applying
[`var_label()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
on it would have an effect on each sub-column. To change a variable
label to the df-column itself, you could use
[`label_attribute()`](https://larmarange.github.io/labelled/dev/reference/var_label.md).

``` r

label_attribute(d$Petal) <- "correct label for Petal"
str(d)
```

    ## tibble [150 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Species: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Sepal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##   .. ..- attr(*, "label")= chr "Length of the sepal"
    ##   ..$ Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##  $ Petal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##   .. ..- attr(*, "label")= chr "wrong label for Petal"
    ##   ..$ Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##   .. ..- attr(*, "label")= chr "wrong label for Petal"
    ##   ..- attr(*, "label")= chr "correct label for Petal"

On the other hand,
[`set_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
works differently, as the primary intention of this function is to work
on the columns of a tibble.

``` r

d <- d %>% set_variable_labels(Sepal = "Label of the Sepal df-column")
str(d)
```

    ## tibble [150 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Species: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Sepal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##   .. ..- attr(*, "label")= chr "Length of the sepal"
    ##   ..$ Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##   ..- attr(*, "label")= chr "Label of the Sepal df-column"
    ##  $ Petal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##   .. ..- attr(*, "label")= chr "wrong label for Petal"
    ##   ..$ Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##   .. ..- attr(*, "label")= chr "wrong label for Petal"
    ##   ..- attr(*, "label")= chr "correct label for Petal"

This is equivalent to:

``` r

var_label(d) <- list(Sepal = "Label of the Sepal df-column")
str(d)
```

    ## tibble [150 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Species: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Sepal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##   .. ..- attr(*, "label")= chr "Length of the sepal"
    ##   ..$ Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##   ..- attr(*, "label")= chr "Label of the Sepal df-column"
    ##  $ Petal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##   .. ..- attr(*, "label")= chr "wrong label for Petal"
    ##   ..$ Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##   .. ..- attr(*, "label")= chr "wrong label for Petal"
    ##   ..- attr(*, "label")= chr "correct label for Petal"

To use
[`set_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
on sub-columns, you should use this syntax:

``` r

d$Petal <- d$Petal %>%
  set_variable_labels(
    Length = "Petal length",
    Width = "Petal width"
  )
str(d)
```

    ## tibble [150 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Species: Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Sepal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##   .. ..- attr(*, "label")= chr "Length of the sepal"
    ##   ..$ Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##   ..- attr(*, "label")= chr "Label of the Sepal df-column"
    ##  $ Petal  : tibble [150 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##   .. ..- attr(*, "label")= chr "Petal length"
    ##   ..$ Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##   .. ..- attr(*, "label")= chr "Petal width"
    ##   ..- attr(*, "label")= chr "correct label for Petal"

If you want to get the list of variable labels of a tibble, by default
[`var_label()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
or
[`get_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
will return the labels of the first level of columns.

``` r

d %>% get_variable_labels()
```

    ## $Species
    ## NULL
    ## 
    ## $Sepal
    ## [1] "Label of the Sepal df-column"
    ## 
    ## $Petal
    ## [1] "correct label for Petal"

To obtain the list of variable labels for sub-columns, you could use
`recurse = TRUE`:

``` r

d %>% get_variable_labels(recurse = TRUE)
```

    ## $Species
    ## NULL
    ## 
    ## $Sepal
    ## $Sepal$Length
    ## [1] "Length of the sepal"
    ## 
    ## $Sepal$Width
    ## NULL
    ## 
    ## 
    ## $Petal
    ## $Petal$Length
    ## [1] "Petal length"
    ## 
    ## $Petal$Width
    ## [1] "Petal width"

``` r

d %>%
  get_variable_labels(
    recurse = TRUE,
    null_action = "fill",
    unlist = TRUE
  )
```

    ##               Species          Sepal.Length           Sepal.Width 
    ##             "Species" "Length of the sepal"               "Width" 
    ##          Petal.Length           Petal.Width 
    ##        "Petal length"         "Petal width"
