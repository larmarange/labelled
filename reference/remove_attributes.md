# Remove attributes

This function removes specified attributes. When applied to a
data.frame, it will also remove recursively the specified attributes to
each column of the data.frame.

## Usage

``` r
remove_attributes(x, attributes)
```

## Arguments

- x:

  an object

- attributes:

  a character vector indicating attributes to remove

## Examples

``` r
if (FALSE) { # \dontrun{
library(haven)
path <- system.file("examples", "iris.sav", package = "haven")
d <- read_sav(path)
str(d)
d <- remove_attributes(d, "format.spss")
str(d)
} # }
```
