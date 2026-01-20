# Update labelled data to last version

Labelled data imported with haven version 1.1.2 or before or created
with
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
version 1.1.0 or before was using "labelled" and "labelled_spss"
classes.

## Usage

``` r
update_labelled(x)

# S3 method for class 'labelled'
update_labelled(x)

# S3 method for class 'haven_labelled_spss'
update_labelled(x)

# S3 method for class 'haven_labelled'
update_labelled(x)

# S3 method for class 'data.frame'
update_labelled(x)
```

## Arguments

- x:

  An object (vector or data.frame) to convert.

## Details

Since version 2.0.0 of these two packages, "haven_labelled" and
"haven_labelled_spss" are used instead.

Since haven 2.3.0, "haven_labelled" class has been evolving using now
vctrs package.

`update_labelled()` convert labelled vectors from the old to the new
classes and to reconstruct all labelled vectors with the last version of
the package.

## See also

[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html),
[`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
