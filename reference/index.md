# Package index

## Manipulating variable labels

Functions to set, manipulate and remove variable labels

- [`var_label()`](https://larmarange.github.io/labelled/reference/var_label.md)
  [`` `var_label<-`() ``](https://larmarange.github.io/labelled/reference/var_label.md)
  [`get_variable_labels()`](https://larmarange.github.io/labelled/reference/var_label.md)
  [`set_variable_labels()`](https://larmarange.github.io/labelled/reference/var_label.md)
  [`label_attribute()`](https://larmarange.github.io/labelled/reference/var_label.md)
  [`get_label_attribute()`](https://larmarange.github.io/labelled/reference/var_label.md)
  [`set_label_attribute()`](https://larmarange.github.io/labelled/reference/var_label.md)
  [`` `label_attribute<-`() ``](https://larmarange.github.io/labelled/reference/var_label.md)
  : Get / Set a variable label
- [`remove_labels()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  [`remove_var_label()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  [`remove_val_labels()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  [`remove_user_na()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  : Remove variable label, value labels and user defined missing values
- [`copy_labels()`](https://larmarange.github.io/labelled/reference/copy_labels.md)
  [`copy_labels_from()`](https://larmarange.github.io/labelled/reference/copy_labels.md)
  : Copy variable and value labels and SPSS-style missing value
- [`update_variable_labels_with()`](https://larmarange.github.io/labelled/reference/update_variable_labels_with.md)
  [`update_value_labels_with()`](https://larmarange.github.io/labelled/reference/update_variable_labels_with.md)
  : Update variable/value labels with a function
- [`dictionary_to_variable_labels()`](https://larmarange.github.io/labelled/reference/dictionary_to_variable_labels.md)
  [`dictionary_to_value_labels()`](https://larmarange.github.io/labelled/reference/dictionary_to_variable_labels.md)
  : Convert a dictionary data frame into a list of variable / value
  labels

## Manipulating value labels

Functions to set, manipulate and remove value labels

- [`labelled()`](https://haven.tidyverse.org/reference/labelled.html)
  [`is.labelled()`](https://haven.tidyverse.org/reference/labelled.html)
  : Create a labelled vector. (from haven)
- [`val_labels()`](https://larmarange.github.io/labelled/reference/val_labels.md)
  [`` `val_labels<-`() ``](https://larmarange.github.io/labelled/reference/val_labels.md)
  [`val_label()`](https://larmarange.github.io/labelled/reference/val_labels.md)
  [`` `val_label<-`() ``](https://larmarange.github.io/labelled/reference/val_labels.md)
  [`get_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.md)
  [`set_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.md)
  [`add_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.md)
  [`remove_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.md)
  : Get / Set value labels
- [`remove_labels()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  [`remove_var_label()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  [`remove_val_labels()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  [`remove_user_na()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  : Remove variable label, value labels and user defined missing values
- [`sort_val_labels()`](https://larmarange.github.io/labelled/reference/sort_val_labels.md)
  : Sort value labels
- [`val_labels_to_na()`](https://larmarange.github.io/labelled/reference/val_labels_to_na.md)
  : Recode value labels to NA
- [`nolabel_to_na()`](https://larmarange.github.io/labelled/reference/nolabel_to_na.md)
  : Recode values with no label to NA
- [`drop_unused_value_labels()`](https://larmarange.github.io/labelled/reference/drop_unused_value_labels.md)
  : Drop unused value labels
- [`copy_labels()`](https://larmarange.github.io/labelled/reference/copy_labels.md)
  [`copy_labels_from()`](https://larmarange.github.io/labelled/reference/copy_labels.md)
  : Copy variable and value labels and SPSS-style missing value
- [`update_variable_labels_with()`](https://larmarange.github.io/labelled/reference/update_variable_labels_with.md)
  [`update_value_labels_with()`](https://larmarange.github.io/labelled/reference/update_variable_labels_with.md)
  : Update variable/value labels with a function
- [`dictionary_to_variable_labels()`](https://larmarange.github.io/labelled/reference/dictionary_to_variable_labels.md)
  [`dictionary_to_value_labels()`](https://larmarange.github.io/labelled/reference/dictionary_to_variable_labels.md)
  : Convert a dictionary data frame into a list of variable / value
  labels

## Data dictionary

Functions to look for keywords variable names / labels and create a data
dictionary.

- [`look_for()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`lookfor()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`generate_dictionary()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`print(`*`<look_for>`*`)`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`look_for_and_select()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`convert_list_columns_to_character()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`lookfor_to_long_format()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`to_gt()`](https://larmarange.github.io/labelled/reference/look_for.md)
  : Look for keywords variable names and descriptions / Create a data
  dictionary

## Manipulating SPSS style missing values

Functions to set, manipulate and remove SPSS style missing values

- [`labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
  : Labelled vectors for SPSS (from haven)
- [`na_values()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`` `na_values<-`() ``](https://larmarange.github.io/labelled/reference/na_values.md)
  [`na_range()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`` `na_range<-`() ``](https://larmarange.github.io/labelled/reference/na_values.md)
  [`get_na_values()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`get_na_range()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`set_na_values()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`set_na_range()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`is_user_na()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`is_regular_na()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`user_na_to_na()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`user_na_to_regular_na()`](https://larmarange.github.io/labelled/reference/na_values.md)
  [`user_na_to_tagged_na()`](https://larmarange.github.io/labelled/reference/na_values.md)
  : Get / Set SPSS missing values
- [`copy_labels()`](https://larmarange.github.io/labelled/reference/copy_labels.md)
  [`copy_labels_from()`](https://larmarange.github.io/labelled/reference/copy_labels.md)
  : Copy variable and value labels and SPSS-style missing value
- [`remove_labels()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  [`remove_var_label()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  [`remove_val_labels()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  [`remove_user_na()`](https://larmarange.github.io/labelled/reference/remove_labels.md)
  : Remove variable label, value labels and user defined missing values

## Tagged missing values

- [`tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
  [`na_tag()`](https://haven.tidyverse.org/reference/tagged_na.html)
  [`is_tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
  [`format_tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
  [`print_tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
  : "Tagged" missing values (from haven)
- [`unique_tagged_na()`](https://larmarange.github.io/labelled/reference/unique_tagged_na.md)
  [`duplicated_tagged_na()`](https://larmarange.github.io/labelled/reference/unique_tagged_na.md)
  [`order_tagged_na()`](https://larmarange.github.io/labelled/reference/unique_tagged_na.md)
  [`sort_tagged_na()`](https://larmarange.github.io/labelled/reference/unique_tagged_na.md)
  : Unique elements, duplicated, ordering and sorting with tagged NAs
- [`tagged_na_to_user_na()`](https://larmarange.github.io/labelled/reference/tagged_na_to_user_na.md)
  [`tagged_na_to_regular_na()`](https://larmarange.github.io/labelled/reference/tagged_na_to_user_na.md)
  : Convert tagged NAs into user NAs

## Converting

Functions to convert from and to labelled classes

- [`look_for()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`lookfor()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`generate_dictionary()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`print(`*`<look_for>`*`)`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`look_for_and_select()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`convert_list_columns_to_character()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`lookfor_to_long_format()`](https://larmarange.github.io/labelled/reference/look_for.md)
  [`to_gt()`](https://larmarange.github.io/labelled/reference/look_for.md)
  : Look for keywords variable names and descriptions / Create a data
  dictionary
- [`to_character()`](https://larmarange.github.io/labelled/reference/to_character.md)
  : Convert input to a character vector
- [`to_factor()`](https://larmarange.github.io/labelled/reference/to_factor.md)
  [`unlabelled()`](https://larmarange.github.io/labelled/reference/to_factor.md)
  : Convert input to a factor.
- [`to_labelled()`](https://larmarange.github.io/labelled/reference/to_labelled.md)
  [`foreign_to_labelled()`](https://larmarange.github.io/labelled/reference/to_labelled.md)
  [`memisc_to_labelled()`](https://larmarange.github.io/labelled/reference/to_labelled.md)
  : Convert to labelled data
- [`update_labelled()`](https://larmarange.github.io/labelled/reference/update_labelled.md)
  : Update labelled data to last version

## Other functions and methods

- [`is_prefixed()`](https://larmarange.github.io/labelled/reference/is_prefixed.md)
  : Check if a factor is prefixed
- [`recode_if()`](https://larmarange.github.io/labelled/reference/recode_if.md)
  : Recode some values based on condition
- [`recode(`*`<haven_labelled>`*`)`](https://larmarange.github.io/labelled/reference/recode.haven_labelled.md)
  : Recode values
- [`remove_attributes()`](https://larmarange.github.io/labelled/reference/remove_attributes.md)
  : Remove attributes
- [`names_prefixed_by_values()`](https://larmarange.github.io/labelled/reference/names_prefixed_by_values.md)
  : Turn a named vector into a vector of names prefixed by values

## Internal datasets for testing

- [`x_haven_2.0`](https://larmarange.github.io/labelled/reference/test_datasets.md)
  [`x_spss_haven_2.0`](https://larmarange.github.io/labelled/reference/test_datasets.md)
  [`spss_file`](https://larmarange.github.io/labelled/reference/test_datasets.md)
  [`dta_file`](https://larmarange.github.io/labelled/reference/test_datasets.md)
  : Datasets for testing
