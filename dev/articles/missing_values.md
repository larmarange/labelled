# About missing values: regular NAs, tagged NAs and user NAs

In base **R**, missing values are indicated using the specific value
`NA`. **Regular NAs** could be used with any type of vector (double,
integer, character, factor, Date, etc.).

Other statistical software have implemented ways to differentiate
several types of missing values.

**Stata** and **SAS** have a system of **tagged NAs**, where NA values
are tagged with a letter (from a to z). **SPSS** allows users to
indicate that certain non-missing values should be treated in some
analysis as missing (**user NAs**). The `haven` package implements
**tagged NAs** and **user NAs** in order to keep this information when
importing files from **Stata**, **SAS** or **SPSS**.

``` r

library(labelled)
```

## Tagged NAs

### Creation and tests

**Tagged NAs** are proper `NA` values with a tag attached to them. They
can be created with
[`tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html).
The attached tag should be a single letter, lowercase (a-z) or uppercase
(A-Z).

``` r

x <- c(1:5, tagged_na("a"), tagged_na("z"), NA)
```

For most **R** functions, tagged NAs are just considered as regular NAs.
By default, they are just printed as any other regular NA.

``` r

x
```

    ## [1]  1  2  3  4  5 NA NA NA

``` r

is.na(x)
```

    ## [1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE

To show/print their tags, you need to use
[`na_tag()`](https://haven.tidyverse.org/reference/tagged_na.html),
[`print_tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
or
[`format_tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html).

``` r

na_tag(x)
```

    ## [1] NA  NA  NA  NA  NA  "a" "z" NA

``` r

print_tagged_na(x)
```

    ## [1]     1     2     3     4     5 NA(a) NA(z)    NA

``` r

format_tagged_na(x)
```

    ## [1] "    1" "    2" "    3" "    4" "    5" "NA(a)" "NA(z)" "   NA"

To test if a certain NA is a regular NA or a tagged NA, you should use
[`is_regular_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
or
[`is_tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html).

``` r

is.na(x)
```

    ## [1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE

``` r

is_tagged_na(x)
```

    ## [1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE

``` r

# You can test for specific tagged NAs with the second argument
is_tagged_na(x, "a")
```

    ## [1] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE

``` r

is_regular_na(x)
```

    ## [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE

Tagged NAs could be defined **only** for double vectors. If you add a
tagged NA to a character vector, it will be converted into a regular NA.
If you add a tagged NA to an integer vector, the vector will be
converted into a double vector.

``` r

y <- c("a", "b", tagged_na("z"))
y
```

    ## [1] "a" "b" NA

``` r

is_tagged_na(y)
```

    ## [1] FALSE FALSE FALSE

``` r

format_tagged_na(y)
```

    ## Error:
    ## ! `x` must be a double vector

``` r

z <- c(1L, 2L, tagged_na("a"))
typeof(z)
```

    ## [1] "double"

``` r

format_tagged_na(z)
```

    ## [1] "    1" "    2" "NA(a)"

### Unique values, duplicates and sorting with tagged NAs

By default, functions such as
[`base::unique()`](https://rdrr.io/r/base/unique.html),
[`base::duplicated()`](https://rdrr.io/r/base/duplicated.html),
[`base::order()`](https://rdrr.io/r/base/order.html) or
[`base::sort()`](https://rdrr.io/r/base/sort.html) will treat tagged NAs
as the same thing as a regular NA. You can use
[`unique_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/unique_tagged_na.md),
[`duplicated_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/unique_tagged_na.md),
[`order_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/unique_tagged_na.md)
and
[`sort_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/unique_tagged_na.md)
as alternatives that will treat two tagged NAs with different tags as
separate values.

``` r

x <- c(1, 2, tagged_na("a"), 1, tagged_na("z"), 2, tagged_na("a"), NA)
x %>% print_tagged_na()
```

    ## [1]     1     2 NA(a)     1 NA(z)     2 NA(a)    NA

``` r

unique(x) %>% print_tagged_na()
```

    ## [1]     1     2 NA(a)

``` r

unique_tagged_na(x) %>% print_tagged_na()
```

    ## [1]     1     2 NA(a) NA(z)    NA

``` r

duplicated(x)
```

    ## [1] FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE

``` r

duplicated_tagged_na(x)
```

    ## [1] FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE

``` r

sort(x, na.last = TRUE) %>% print_tagged_na()
```

    ## [1]     1     1     2     2 NA(a) NA(z) NA(a)    NA

``` r

sort_tagged_na(x) %>% print_tagged_na()
```

    ## [1]     1     1     2     2 NA(a) NA(a) NA(z)    NA

### Tagged NAs and value labels

It is possible to define value labels for tagged NAs.

``` r

x <- c(1, 0, 1, tagged_na("r"), 0, tagged_na("d"), tagged_na("z"), NA)
val_labels(x) <- c(
  no = 0, yes = 1,
  "don't know" = tagged_na("d"),
  refusal = tagged_na("r")
)
x
```

    ## <labelled<double>[8]>
    ## [1]     1     0     1 NA(r)     0 NA(d) NA(z)    NA
    ## 
    ## Labels:
    ##  value      label
    ##      0         no
    ##      1        yes
    ##  NA(d) don't know
    ##  NA(r)    refusal

When converting such labelled vector into factor, tagged NAs are, by
default, converted into regular NAs (it is not possible to define tagged
NAs with factors).

``` r

to_factor(x)
```

    ## [1] yes  no   yes  <NA> no   <NA> <NA> <NA>
    ## Levels: no yes

However, the option `explicit_tagged_na` of
[`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
allows to transform tagged NAs into explicit factor levels.

``` r

to_factor(x, explicit_tagged_na = TRUE)
```

    ## [1] yes        no         yes        refusal    no         don't know NA(z)     
    ## [8] <NA>      
    ## Levels: no yes don't know refusal NA(z)

``` r

to_factor(x, levels = "prefixed", explicit_tagged_na = TRUE)
```

    ## [1] [1] yes            [0] no             [1] yes            [NA(r)] refusal   
    ## [5] [0] no             [NA(d)] don't know [NA(z)] NA(z)      <NA>              
    ## Levels: [0] no [1] yes [NA(d)] don't know [NA(r)] refusal [NA(z)] NA(z)

### Conversion into user NAs

Tagged NAs can be converted into user NAs with
[`tagged_na_to_user_na()`](https://larmarange.github.io/labelled/dev/reference/tagged_na_to_user_na.md).

``` r

tagged_na_to_user_na(x)
```

    ## <labelled_spss<double>[8]>
    ## [1]  1  0  1  3  0  2  4 NA
    ## Missing range:  [2, 4]
    ## 
    ## Labels:
    ##  value      label
    ##      0         no
    ##      1        yes
    ##      2 don't know
    ##      3    refusal
    ##      4      NA(z)

``` r

tagged_na_to_user_na(x, user_na_start = 10)
```

    ## <labelled_spss<double>[8]>
    ## [1]  1  0  1 11  0 10 12 NA
    ## Missing range:  [10, 12]
    ## 
    ## Labels:
    ##  value      label
    ##      0         no
    ##      1        yes
    ##     10 don't know
    ##     11    refusal
    ##     12      NA(z)

Use
[`tagged_na_to_regular_na()`](https://larmarange.github.io/labelled/dev/reference/tagged_na_to_user_na.md)
to convert tagged NAs into regular NAs.

``` r

tagged_na_to_regular_na(x)
```

    ## <labelled<double>[8]>
    ## [1]  1  0  1 NA  0 NA NA NA
    ## 
    ## Labels:
    ##  value label
    ##      0    no
    ##      1   yes

``` r

tagged_na_to_regular_na(x) %>% is_tagged_na()
```

    ## [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

## User NAs

`haven` introduced an `haven_labelled_spss` class to deal with user
defined missing values in a similar way as **SPSS**. In such case,
additional attributes will be used to indicate with values should be
considered as missing, but such values will not be stored as internal
`NA` values. You should note that most R function will not take this
information into account. Therefore, you will have to convert missing
values into `NA` if required before analysis. These defined missing
values could co-exist with internal `NA` values.

### Creation

User NAs could be created directly with
[`labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html).
You can also manipulate them with
[`na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
and
[`na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md).

``` r

v <- labelled(c(1, 2, 3, 9, 1, 3, 2, NA), c(yes = 1, no = 3, "don't know" = 9))
v
```

    ## <labelled<double>[8]>
    ## [1]  1  2  3  9  1  3  2 NA
    ## 
    ## Labels:
    ##  value      label
    ##      1        yes
    ##      3         no
    ##      9 don't know

``` r

na_values(v) <- 9
v
```

    ## <labelled_spss<double>[8]>
    ## [1]  1  2  3  9  1  3  2 NA
    ## Missing values: 9
    ## 
    ## Labels:
    ##  value      label
    ##      1        yes
    ##      3         no
    ##      9 don't know

``` r

na_values(v) <- NULL
v
```

    ## <labelled<double>[8]>
    ## [1]  1  2  3  9  1  3  2 NA
    ## 
    ## Labels:
    ##  value      label
    ##      1        yes
    ##      3         no
    ##      9 don't know

``` r

na_range(v) <- c(5, Inf)
na_range(v)
```

    ## [1]   5 Inf

``` r

v
```

    ## <labelled_spss<double>[8]>
    ## [1]  1  2  3  9  1  3  2 NA
    ## Missing range:  [5, Inf]
    ## 
    ## Labels:
    ##  value      label
    ##      1        yes
    ##      3         no
    ##      9 don't know

NB: you cant also use
[`set_na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
and
[`set_na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
for a `dplyr`-like syntax.

``` r

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r

# setting value labels and user NAs
df <- tibble(s1 = c("M", "M", "F", "F"), s2 = c(1, 1, 2, 9)) %>%
  set_value_labels(s2 = c(yes = 1, no = 2)) %>%
  set_na_values(s2 = 9)
df$s2
```

    ## <labelled_spss<double>[4]>
    ## [1] 1 1 2 9
    ## Missing values: 9
    ## 
    ## Labels:
    ##  value label
    ##      1   yes
    ##      2    no

``` r

# removing user NAs
df <- df %>% set_na_values(s2 = NULL)
df$s2
```

    ## <labelled<double>[4]>
    ## [1] 1 1 2 9
    ## 
    ## Labels:
    ##  value label
    ##      1   yes
    ##      2    no

### Tests

Note that [`is.na()`](https://rdrr.io/r/base/NA.html) will return `TRUE`
for user NAs. Use
[`is_user_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
to test if a specific value is a user NA and
[`is_regular_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
to test if it is a regular NA.

``` r

v
```

    ## <labelled_spss<double>[8]>
    ## [1]  1  2  3  9  1  3  2 NA
    ## Missing range:  [5, Inf]
    ## 
    ## Labels:
    ##  value      label
    ##      1        yes
    ##      3         no
    ##      9 don't know

``` r

is.na(v)
```

    ## [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE

``` r

is_user_na(v)
```

    ## [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE

``` r

is_regular_na(v)
```

    ## [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE

### Conversion

For most **R** functions, user NAs values are **still** regular values.

``` r

x <- c(1:5, 11:15)
na_range(x) <- c(10, Inf)
val_labels(x) <- c("dk" = 11, "refused" = 15)
x
```

    ## <labelled_spss<integer>[10]>
    ##  [1]  1  2  3  4  5 11 12 13 14 15
    ## Missing range:  [10, Inf]
    ## 
    ## Labels:
    ##  value   label
    ##     11      dk
    ##     15 refused

``` r

mean(x)
```

    ## [1] 8

You can convert user NAs into regular NAs with
[`user_na_to_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
or
[`user_na_to_regular_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
(both functions are identical).

``` r

user_na_to_na(x)
```

    ## <labelled<integer>[10]>
    ##  [1]  1  2  3  4  5 NA NA NA NA NA

``` r

mean(user_na_to_na(x), na.rm = TRUE)
```

    ## [1] 3

Alternatively, if the vector is numeric, you can convert user NAs into
tagged NAs with
[`user_na_to_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md).

``` r

user_na_to_tagged_na(x)
```

    ## ℹ `x` has been converted into a double vector.

    ## <labelled<double>[10]>
    ##  [1]     1     2     3     4     5 NA(a) NA(b) NA(c) NA(d) NA(e)
    ## 
    ## Labels:
    ##  value   label
    ##  NA(a)      dk
    ##  NA(e) refused

``` r

mean(user_na_to_tagged_na(x), na.rm = TRUE)
```

    ## ℹ `x` has been converted into a double vector.

    ## [1] 3

Finally, you can also remove user NAs definition without converting
these values to `NA`, using
[`remove_user_na()`](https://larmarange.github.io/labelled/dev/reference/remove_labels.md).

``` r

remove_user_na(x)
```

    ## <labelled<integer>[10]>
    ##  [1]  1  2  3  4  5 11 12 13 14 15
    ## 
    ## Labels:
    ##  value   label
    ##     11      dk
    ##     15 refused

``` r

mean(remove_user_na(x))
```

    ## [1] 8
