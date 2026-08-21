# Convert to labelled data

Convert a factor or data imported with foreign or memisc to labelled
data.

## Usage

``` r
to_labelled(x, ...)

# S3 method for class 'data.frame'
to_labelled(x, ...)

# S3 method for class 'list'
to_labelled(x, ...)

# S3 method for class 'data.set'
to_labelled(x, ...)

# S3 method for class 'importer'
to_labelled(x, ...)

foreign_to_labelled(x)

memisc_to_labelled(x)

# S3 method for class 'factor'
to_labelled(x, labels = NULL, .quiet = FALSE, ...)
```

## Arguments

- x:

  Factor or dataset to convert to labelled data frame

- ...:

  Not used

- labels:

  When converting a factor only: an optional named vector indicating how
  factor levels should be coded. If a factor level is not found in
  `labels`, it will be converted to `NA`.

- .quiet:

  do not display warnings for prefixed factors with duplicated codes

## Value

A tbl data frame or a labelled vector.

## Details

`to_labelled()` is a general wrapper calling the appropriate
sub-functions.

`memisc_to_labelled()` converts a
[`memisc::data.set()`](https://melff.github.io/memisc/reference/dataSets.html)\]\`
object created with memisc package to a labelled data frame.

`foreign_to_labelled()` converts data imported with
[`foreign::read.spss()`](https://rdrr.io/pkg/foreign/man/read.spss.html)
or
[`foreign::read.dta()`](https://rdrr.io/pkg/foreign/man/read.dta.html)
from foreign package to a labelled data frame, i.e. using
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html).
Factors will not be converted. Therefore, you should use
`use.value.labels = FALSE` when importing with
[`foreign::read.spss()`](https://rdrr.io/pkg/foreign/man/read.spss.html)
or `convert.factors = FALSE` when importing with
[`foreign::read.dta()`](https://rdrr.io/pkg/foreign/man/read.dta.html).

To convert correctly defined missing values imported with
[`foreign::read.spss()`](https://rdrr.io/pkg/foreign/man/read.spss.html),
you should have used `to.data.frame = FALSE` and `use.missings = FALSE`.
If you used the option `to.data.frame = TRUE`, meta data describing
missing values will not be attached to the import. If you used
`use.missings = TRUE`, missing values would have been converted to `NA`.

So far, missing values defined in **Stata** are always imported as `NA`
by
[`foreign::read.dta()`](https://rdrr.io/pkg/foreign/man/read.dta.html)
and could not be retrieved by `foreign_to_labelled()`.

If you convert a labelled vector into a factor with prefix, i.e. by
using
[`to_factor(levels = "prefixed")`](https://larmarange.github.io/labelled/reference/to_factor.md),
`to_labelled.factor()` is able to reconvert it to a labelled vector with
same values and labels.

## See also

[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html),
[`foreign::read.spss()`](https://rdrr.io/pkg/foreign/man/read.spss.html),
[`foreign::read.dta()`](https://rdrr.io/pkg/foreign/man/read.dta.html),
[`memisc::data.set()`](https://melff.github.io/memisc/reference/dataSets.html),
[`memisc::importer`](https://melff.github.io/memisc/reference/importers.html),
[`to_factor()`](https://larmarange.github.io/labelled/reference/to_factor.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# from foreign
library(foreign)
sav <- system.file("files", "electric.sav", package = "foreign")
df <- to_labelled(read.spss(
  sav,
  to.data.frame = FALSE,
  use.value.labels = FALSE,
  use.missings = FALSE
))

# from memisc
library(memisc)
nes1948.por <- UnZip("anes/NES1948.ZIP", "NES1948.POR", package = "memisc")
nes1948 <- spss.portable.file(nes1948.por)
ds <- as.data.set(nes1948)
df <- to_labelled(ds)
} # }

# Converting factors to labelled vectors
f <- factor(
  c("yes", "yes", "no", "no", "don't know", "no", "yes", "don't know")
)
to_labelled(f)
#> <labelled<double>[8]>
#> [1] 3 3 2 2 1 2 3 1
#> 
#> Labels:
#>  value      label
#>      1 don't know
#>      2         no
#>      3        yes
to_labelled(f, c("yes" = 1, "no" = 2, "don't know" = 9))
#> <labelled<double>[8]>
#> [1] 1 1 2 2 9 2 1 9
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      2         no
#>      9 don't know
to_labelled(f, c("yes" = 1, "no" = 2))
#> <labelled<double>[8]>
#> [1]  1  1  2  2 NA  2  1 NA
#> 
#> Labels:
#>  value label
#>      1   yes
#>      2    no
to_labelled(f, c("yes" = "Y", "no" = "N", "don't know" = "DK"))
#> <labelled<character>[8]>
#> [1] Y  Y  N  N  DK N  Y  DK
#> 
#> Labels:
#>  value      label
#>      Y        yes
#>      N         no
#>     DK don't know

s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
labels <- val_labels(s1)
f1 <- to_factor(s1)
f1
#> [1] Male   Male   Female
#> Levels: Male Female

to_labelled(f1)
#> <labelled<double>[3]>
#> [1] 1 1 2
#> 
#> Labels:
#>  value  label
#>      1   Male
#>      2 Female
identical(s1, to_labelled(f1))
#> [1] FALSE
to_labelled(f1, labels)
#> <labelled<character>[3]>
#> [1] M M F
#> 
#> Labels:
#>  value  label
#>      M   Male
#>      F Female
identical(s1, to_labelled(f1, labels))
#> [1] TRUE

l <- labelled(
  c(1, 1, 2, 2, 9, 2, 1, 9),
  c("yes" = 1, "no" = 2, "don't know" = 9)
)
f <- to_factor(l, levels = "p")
f
#> [1] [1] yes        [1] yes        [2] no         [2] no         [9] don't know
#> [6] [2] no         [1] yes        [9] don't know
#> Levels: [1] yes [2] no [9] don't know
to_labelled(f)
#> <labelled<double>[8]>
#> [1] 1 1 2 2 9 2 1 9
#> 
#> Labels:
#>  value      label
#>      1        yes
#>      2         no
#>      9 don't know
identical(to_labelled(f), l)
#> [1] TRUE
```
