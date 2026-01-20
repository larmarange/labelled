# Changelog

## labelled (development version)

**Bug fix**

- `value_labels()` works on empty vector, i.e. a logical vector
  containing only `NA` values
  ([\#192](https://github.com/larmarange/labelled/issues/192))

## labelled 2.16.0

CRAN release: 2025-10-22

**New features**

- new function
  [`to_gt()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  for converting the result of
  [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  into a nicely formatted table
  ([\#189](https://github.com/larmarange/labelled/issues/189))
- new functions
  [`dictionary_to_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/dictionary_to_variable_labels.md)
  and
  [`dictionary_to_value_labels()`](https://larmarange.github.io/labelled/dev/reference/dictionary_to_variable_labels.md)
  to convert a dictionary data frame into a list of variable / value
  labels ([\#183](https://github.com/larmarange/labelled/issues/183))
- new `.overwrite` argument for
  [`set_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
  and
  [`set_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md)
  ([\#183](https://github.com/larmarange/labelled/issues/183))
- new `sep` argument for
  [`names_prefixed_by_values()`](https://larmarange.github.io/labelled/dev/reference/names_prefixed_by_values.md)
- new arguments (`sep_value_labels` and `sep_other`) for
  [`convert_list_columns_to_character()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  and
  [`lookfor_to_long_format()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)

**Bug fix**

- [`copy_labels()`](https://larmarange.github.io/labelled/dev/reference/copy_labels.md)
  is now less restrictive and accept tibble with list columns
  ([\#187](https://github.com/larmarange/labelled/issues/187))

## labelled 2.15.0

CRAN release: 2025-09-16

**New features**

- [labelled](https://larmarange.github.io/labelled/) functions are now
  compatible with survey design objects created with the
  [survey](http://r-survey.r-forge.r-project.org/survey/) package
  ([\#174](https://github.com/larmarange/labelled/issues/174))
- argument `user_na_to_na` has been added to
  [`to_factor.data.frame()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
  ([\#178](https://github.com/larmarange/labelled/issues/178))

## labelled 2.14.1

CRAN release: 2025-05-06

**Bug fix**

- fix a bug introduced in 2.14.0 in
  [`set_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
  when using `.labels` argument
  ([\#171](https://github.com/larmarange/labelled/issues/171))

## labelled 2.14.0

CRAN release: 2025-01-08

**New features**

- in
  [`update_variable_labels_with()`](https://larmarange.github.io/labelled/dev/reference/update_variable_labels_with.md),
  it is now possible to access the variable name inside `.fn` by using
  [`names()`](https://rdrr.io/r/base/names.html)
  ([\#163](https://github.com/larmarange/labelled/issues/163))
- [`var_label()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
  gets new options `"na"` and `"empty"` for `null_action`

**Improvements**

- systematic use of [cli](https://cli.r-lib.org) for errors, warnings
  and messages
  ([\#167](https://github.com/larmarange/labelled/issues/167))

## labelled 2.13.0

CRAN release: 2024-04-23

**New features**

- add a `null_action` argument to
  [`val_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`val_label()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md)
  and a `.null_action` argument to
  [`set_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`add_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md)
  and
  [`remove_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md)
  ([\#145](https://github.com/larmarange/labelled/issues/145))
- new functions
  [`update_variable_labels_with()`](https://larmarange.github.io/labelled/dev/reference/update_variable_labels_with.md)
  and
  [`update_value_labels_with()`](https://larmarange.github.io/labelled/dev/reference/update_variable_labels_with.md)
  allowing to update variable/value labels with a custom function
  ([\#153](https://github.com/larmarange/labelled/issues/153))

**Bug fix**

- avoid an error with
  [`print.look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  when console pane is physically shrunk too small
  ([\#148](https://github.com/larmarange/labelled/issues/148))
- fix in
  [`recode.haven_labelled()`](https://larmarange.github.io/labelled/dev/reference/recode.haven_labelled.md)
  when `.x` contains `NA` and `.combine_value_labels = TRUE`
  ([\#151](https://github.com/larmarange/labelled/issues/151))
- produce an error when trying to assign value labels to a Date vector
  ([\#156](https://github.com/larmarange/labelled/issues/156))

## labelled 2.12.0

CRAN release: 2023-06-21

**New features**

- support of variable labels for packed columns, see dedicated vignette
  ([\#142](https://github.com/larmarange/labelled/issues/142))
- new helpers
  [`label_attribute()`](https://larmarange.github.io/labelled/dev/reference/var_label.md),
  [`get_label_attribute()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
  and
  [`set_label_attribute()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
  to manipulate the “label” attribute on any object
  ([\#142](https://github.com/larmarange/labelled/issues/142))
- new functions
  [`get_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/var_label.md),
  [`get_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`get_na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  and
  [`get_na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  identical to
  [`var_label()`](https://larmarange.github.io/labelled/dev/reference/var_label.md),
  [`val_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  and
  [`na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md),
  respectively
- [`to_character()`](https://larmarange.github.io/labelled/dev/reference/to_character.md)
  method for data frames
  ([\#140](https://github.com/larmarange/labelled/issues/140))

## labelled 2.11.0

CRAN release: 2023-04-11

**Improvements**

- [`set_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`add_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`remove_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`set_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/var_label.md),
  [`set_na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  and
  [`set_na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  can now be applied on a vector
  ([\#126](https://github.com/larmarange/labelled/issues/126))
- new argument `null_action` for
  [`var_label()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
  when applied on a data frame
  ([\#131](https://github.com/larmarange/labelled/issues/131))
- [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  now returns `"missing"` (number of `NA`s) by default
  ([\#133](https://github.com/larmarange/labelled/issues/133))

**Bug fixes**

- bug fix in
  [`print.look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  ([\#135](https://github.com/larmarange/labelled/issues/135))
- bug fix in
  [`unlabelled()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
  for classic vectors, now remained unchanged
  ([\#137](https://github.com/larmarange/labelled/issues/137))

## labelled 2.10.0

CRAN release: 2022-09-14

- [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  now accepts `survey` objects
  ([\#121](https://github.com/larmarange/labelled/issues/121))

## labelled 2.9.1

CRAN release: 2022-05-05

- improved error messages for missing variable names
  ([\#118](https://github.com/larmarange/labelled/issues/118),
  [@ajb5d](https://github.com/ajb5d))
- better implementation of
  [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  when no keyword is provided
  ([\#116](https://github.com/larmarange/labelled/issues/116))
- bug fix in
  [`user_na_to_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  ([\#114](https://github.com/larmarange/labelled/issues/114))

## labelled 2.9.0

CRAN release: 2021-10-29

**look_for() improvements:**

- new function
  [`look_for_and_select()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  ([\#87](https://github.com/larmarange/labelled/issues/87))
- [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  can now search within factor levels and value labels
  ([\#104](https://github.com/larmarange/labelled/issues/104))

**improvements for tagged NAs:**

- better printing of value labels
  ([\#89](https://github.com/larmarange/labelled/issues/89))
- new functions
  [`user_na_to_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md),
  [`tagged_na_to_user_na()`](https://larmarange.github.io/labelled/dev/reference/tagged_na_to_user_na.md)
  and
  [`tagged_na_to_regular_na()`](https://larmarange.github.io/labelled/dev/reference/tagged_na_to_user_na.md)
- new option `explicit_tagged_na` in
  [`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
  and
  [`to_character()`](https://larmarange.github.io/labelled/dev/reference/to_character.md)
- new functions
  [`unique_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/unique_tagged_na.md),
  [`duplicated_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/unique_tagged_na.md),
  [`order_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/unique_tagged_na.md),
  [`sort_tagged_na()`](https://larmarange.github.io/labelled/dev/reference/unique_tagged_na.md)
  ([\#90](https://github.com/larmarange/labelled/issues/90),
  [\#91](https://github.com/larmarange/labelled/issues/91))

**other improvements:**

- new functions
  [`is_user_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  and
  [`is_regular_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
- new set of unit tests
  ([\#99](https://github.com/larmarange/labelled/issues/99))
- trying to apply a value label,
  [`na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  or
  [`na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  to a factor will now produce an error
- bug fix in
  [`foreign_to_labelled()`](https://larmarange.github.io/labelled/dev/reference/to_labelled.md)
  for Stata files
  ([\#100](https://github.com/larmarange/labelled/issues/100))

## labelled 2.8.0

CRAN release: 2021-03-08

- new helper
  [`recode_if()`](https://larmarange.github.io/labelled/dev/reference/recode_if.md)
  for recoding values based on condition, variable and value labels
  being preserved
  ([\#82](https://github.com/larmarange/labelled/issues/82))
- [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  could be time consuming for big data frames. Now, by default, only
  basic details of each variable are computed. You can compute all
  details with `details = "full"`
  ([\#77](https://github.com/larmarange/labelled/issues/77))
- printing of
  [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  results has been updated and do not rely anymore on `pillar`
  ([\#85](https://github.com/larmarange/labelled/issues/85))
- [`to_labelled()`](https://larmarange.github.io/labelled/dev/reference/to_labelled.md)
  can properly manage factors whose levels are coded as “\[code\]
  level”, as produced by `to_factor(levels = "prefixed")`
  ([\#74](https://github.com/larmarange/labelled/issues/74)
  [@courtiol](https://github.com/courtiol))
- new function
  [`is_prefixed()`](https://larmarange.github.io/labelled/dev/reference/is_prefixed.md)
  to check if a factor is prefixed
- bug fix for `na_range<-` and `na_values<-` when applied to a
  data.frame ([\#80](https://github.com/larmarange/labelled/issues/80))

## labelled 2.7.0

CRAN release: 2020-09-21

- a `.values` argument has been added to
  [`set_na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  and
  [`set_na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md),
  allowing to pass a list of values
- a `.strict` option has been added to
  [`set_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/var_label.md),
  [`set_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`add_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`remove_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`set_na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  and
  [`set_na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md),
  allowing to pass values for columns not observed in the data (it could
  be useful for using a same list of labels for several data.frame
  sharing some variables)
  ([\#70](https://github.com/larmarange/labelled/issues/70))
- [`copy_labels()`](https://larmarange.github.io/labelled/dev/reference/copy_labels.md)
  is less restrictive for non labelled vectors, copying variable label
  even if the two vectors are not of the same type
  ([\#71](https://github.com/larmarange/labelled/issues/71))
- a `.strict` option has been added to
  [`copy_labels()`](https://larmarange.github.io/labelled/dev/reference/copy_labels.md)
  ([\#71](https://github.com/larmarange/labelled/issues/71))

## labelled 2.6.0

CRAN release: 2020-08-27

- [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  has been redesigned:
  - [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
    now returns a tibble
  - columns with multiple values for each variable are now stored as
    named lists
  - a print method has been added for a clearer presentation of results
    in the console
  - use
    [`lookfor_to_long_format()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
    to convert results with one row per factor level and per value label
  - use
    [`convert_list_columns_to_character()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
    to convert list columns to simpler character vectors
  - [`generate_dictionary()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
    is an equivalent of
    [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
- `set_variable_labels`, `set_value_labels`, `add_value_labels`, and
  `remove_value_labels` now accept “tidy dots”
  ([\#67](https://github.com/larmarange/labelled/issues/67)
  [@psanker](https://github.com/psanker))
- new function
  [`names_prefixed_by_values()`](https://larmarange.github.io/labelled/dev/reference/names_prefixed_by_values.md)
  to get the names of a vector prefixed by their corresponding value

## labelled 2.5.0

CRAN release: 2020-06-17

- new `.keep_value_labels` argument for
  [`recode.haven_labelled()`](https://larmarange.github.io/labelled/dev/reference/recode.haven_labelled.md)
- new `.combine_value_labels` argument for
  [`recode.haven_labelled()`](https://larmarange.github.io/labelled/dev/reference/recode.haven_labelled.md)
  ([\#61](https://github.com/larmarange/labelled/issues/61))
- new
  [`drop_unused_value_labels()`](https://larmarange.github.io/labelled/dev/reference/drop_unused_value_labels.md)
  method
- an additional `.labels` argument for
  [`set_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md)
- `user_na_to_na` argument has been added to
  [`to_character.haven_labelled()`](https://larmarange.github.io/labelled/dev/reference/to_character.md)
- `%>%` is now imported from `dplyr`
- a cheatsheet has been added
  ([\#47](https://github.com/larmarange/labelled/issues/47))
- internal documentation is now using **roxygen2** markdown support

## labelled 2.4.0

CRAN release: 2020-05-25

- fixes for haven 2.3.0
  ([\#59](https://github.com/larmarange/labelled/issues/59) by
  [@hadley](https://github.com/hadley))
- correct re-export of functions from `haven`
- [`update_labelled()`](https://larmarange.github.io/labelled/dev/reference/update_labelled.md)
  has been improved to allow to reconstruct all labelled vectors created
  with a previous version of `haven`

## labelled 2.3.1

CRAN release: 2020-04-29

- an additional argument `keep_var_label` for
  [`remove_labels()`](https://larmarange.github.io/labelled/dev/reference/remove_labels.md)
- bug fix for
  [`unlabelled()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
  when applied on a vector
- when using `unclass = TRUE` with
  [`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md),
  attributes are not removed anymore

## labelled 2.3.0

CRAN release: 2020-04-27

- new function
  [`unlabelled()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)

## labelled 2.2.2

CRAN release: 2020-02-03

- bug fix for
  [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  ([\#52](https://github.com/larmarange/labelled/issues/52) by
  [@NoahMarconi](https://github.com/NoahMarconi))
- bug fix in
  [`val_labels_to_na()`](https://larmarange.github.io/labelled/dev/reference/val_labels_to_na.md)
  documentation

## labelled 2.2.1

CRAN release: 2019-05-26

- bug fix for
  [`na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  and
  [`na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md):
  variable labels are now preserved
  ([\#48](https://github.com/larmarange/labelled/issues/48), thanks to
  [@mspittler](https://github.com/mspittler))

## labelled 2.2.0

CRAN release: 2019-04-16

- new function
  [`copy_labels_from()`](https://larmarange.github.io/labelled/dev/reference/copy_labels.md),
  compliant with `dplyr` syntax
- [`update_labelled()`](https://larmarange.github.io/labelled/dev/reference/update_labelled.md)
  is now more strict
  ([\#42](https://github.com/larmarange/labelled/issues/42) by
  [@iago-pssjd](https://github.com/iago-pssjd))
- new functions
  [`look_for()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  and
  [`lookfor()`](https://larmarange.github.io/labelled/dev/reference/look_for.md)
  imported from `questionr`
  ([\#44](https://github.com/larmarange/labelled/issues/44))
- new `unlist` option for
  [`var_label()`](https://larmarange.github.io/labelled/dev/reference/var_label.md)
- [`tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
  and similar functions are now imported from `haven`

## labelled 2.1.0

CRAN release: 2019-02-25

- [`var_label()`](https://larmarange.github.io/labelled/dev/reference/var_label.md),
  applied to a data.frame, now accepts a character vector of same length
  as the number of columns.
- `set_variable_labels` has a new `.labels` argument.
- New `unclass` option in
  [`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md),
  to be used when `strict = TRUE`
  ([\#36](https://github.com/larmarange/labelled/issues/36))
- Following `haven` version 2.1.0, it is not mandatory anymore to define
  a value label before defining a SPSS style missing value.
  [`labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html),
  [`na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  and
  [`na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md)
  have been updated accordingly
  ([\#37](https://github.com/larmarange/labelled/issues/37))

## labelled 2.0.2

CRAN release: 2019-01-18

- [`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
  bug fix then applied on a data.frame
  ([\#33](https://github.com/larmarange/labelled/issues/33))

## labelled 2.0.1

CRAN release: 2018-11-25

- [`update_labelled()`](https://larmarange.github.io/labelled/dev/reference/update_labelled.md)
  bug fix then applied on a data.frame
  ([\#31](https://github.com/larmarange/labelled/issues/31))

## labelled 2.0.0

CRAN release: 2018-11-23

### BREAKING CHANGE

- Following version 2.0.0 of `haven`,
  [`labelled()`](https://haven.tidyverse.org/reference/labelled.html)
  and
  [`labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
  now produce objects with class “haven_labelled” and
  “haven_labelled_spss”, due to conflict between the previous “labelled”
  class and the “labelled” class used by `Hmisc`.
- A new function
  [`update_labelled()`](https://larmarange.github.io/labelled/dev/reference/update_labelled.md)
  could be used to convert data imported with an older version of
  `haven` to the new classes.

### Other changes

- `user_na_to_na` option added to
  [`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
- [`foreign_to_labelled()`](https://larmarange.github.io/labelled/dev/reference/to_labelled.md)
  now import SPSS missing values
  ([\#27](https://github.com/larmarange/labelled/issues/27))
- a `strict` argument added to
  [`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
  ([\#25](https://github.com/larmarange/labelled/issues/25))
- [`remove_attributes()`](https://larmarange.github.io/labelled/dev/reference/remove_attributes.md)
  preserve character vectors
  ([\#30](https://github.com/larmarange/labelled/issues/30))

## labelled 1.1.0

CRAN release: 2018-05-24

- extend
  [`dplyr::recode()`](https://dplyr.tidyverse.org/reference/recode.html)
  method to be compatible with labelled vectors.
- [`copy_labels()`](https://larmarange.github.io/labelled/dev/reference/copy_labels.md)
  now copy also `na_range` and `na_values` attributes.
- new method
  [`remove_attributes()`](https://larmarange.github.io/labelled/dev/reference/remove_attributes.md)

## labelled 1.0.1

CRAN release: 2017-12-19

- bug fix: argument `drop_unused_labels` could now be used with
  [`to_factor.data.frame()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
- new labels argument for
  [`to_labelled()`](https://larmarange.github.io/labelled/dev/reference/to_labelled.md)
  method when applied to a factor
- bug fix: appropriate column names with `data.frame`
  ([\#20](https://github.com/larmarange/labelled/issues/20))

## labelled 1.0.0

CRAN release: 2016-11-20

- now imports `haven`
- new function to deal with user defined missing values (SPSS style):
  [`na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md),
  [`na_range()`](https://larmarange.github.io/labelled/dev/reference/na_values.md),
  [`set_na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md),
  [`set_na_values()`](https://larmarange.github.io/labelled/dev/reference/na_values.md),
  [`remove_user_na()`](https://larmarange.github.io/labelled/dev/reference/remove_labels.md),
  [`user_na_to_na()`](https://larmarange.github.io/labelled/dev/reference/na_values.md).
- [`remove_labels()`](https://larmarange.github.io/labelled/dev/reference/remove_labels.md)
  has been updated.

## labelled 0.2.3

CRAN release: 2016-06-21

- new functions
  [`set_variable_labels()`](https://larmarange.github.io/labelled/dev/reference/var_label.md),
  [`set_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md),
  [`add_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md)
  and
  [`remove_value_labels()`](https://larmarange.github.io/labelled/dev/reference/val_labels.md)
  compatible with `%>%`.
- new functions `remove_val_labels` and
  [`remove_var_label()`](https://larmarange.github.io/labelled/dev/reference/remove_labels.md).
- bug fix in `to_character.labelled()` when applied to data frames.

## labelled 0.2.2

CRAN release: 2016-06-15

- [`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md),
  [`to_character()`](https://larmarange.github.io/labelled/dev/reference/to_character.md)
  and
  [`to_labelled.factor()`](https://larmarange.github.io/labelled/dev/reference/to_labelled.md)
  now preserves variable label.
- bug fix in
  [`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
  when applied to data frames.

## labelled 0.2.0

- Following evolution of `haven`, `labelled` doesn’t support missing
  values anymore
  (cf. <https://github.com/hadley/haven/commit/4b12ff9d51ddb9e7486966b85e0bcff44992904d>)
- New function
  [`to_character()`](https://larmarange.github.io/labelled/dev/reference/to_character.md)
  (cf. <https://github.com/larmarange/labelled/commit/3d32852587bb707d06627e56407eed1c9d5a49de>)
- [`to_factor()`](https://larmarange.github.io/labelled/dev/reference/to_factor.md)
  could now be applied to a data.frame
  (cf. <https://github.com/larmarange/labelled/commit/ce1d750681fe0c9bcd767cb83a8d72ed4c5fc5fb>)
- If `data.table` is available, labelled attribute are now changed by
  *reference*
  (cf. <https://github.com/larmarange/labelled/commit/c8b163f706122844d798e6625779e8a65e5bbf41>)
- [`zap_labels()`](https://haven.tidyverse.org/reference/zap_labels.html)
  added as a synonym of
  [`remove_labels()`](https://larmarange.github.io/labelled/dev/reference/remove_labels.md)
