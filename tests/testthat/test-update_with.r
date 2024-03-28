context("Test update_*_with()")

test_that("update_variable_labels_with() works as expected", {
  df <- iris %>%
    set_variable_labels(
      Sepal.Length = "Length of sepal",
      Sepal.Width = "Width of sepal",
      Petal.Length = "Length of petal",
      Petal.Width = "Width of petal",
      Species = "Species"
    )
  tmp <- df %>%
    update_variable_labels_with(toupper)
  expect_equal(var_label(tmp$Species), "SPECIES")
  expect_equal(var_label(tmp$Sepal.Length), "LENGTH OF SEPAL")
  expect_equal(var_label(tmp$Petal.Width), "WIDTH OF PETAL")

  tmp <- df %>%
    update_variable_labels_with(toupper, .cols = dplyr::starts_with("S"))
  expect_equal(var_label(tmp$Species), "SPECIES")
  expect_equal(var_label(tmp$Sepal.Length), "LENGTH OF SEPAL")
  expect_equal(var_label(tmp$Petal.Width), "Width of petal")
})

test_that("update_value_labels_with() works as expected", {
  df <- iris
  df$Species <- to_labelled(df$Species)
  tmp <- df %>%
    update_value_labels_with(toupper)
  expect_equal(val_label(tmp$Species, 1), "SETOSA")

  tmp <- df %>%
    update_value_labels_with(toupper, .cols = dplyr::starts_with("P"))
  expect_equal(val_label(tmp$Species, 1), "setosa")

  tmp <- df %>%
    update_value_labels_with(casefold, upper = TRUE)
  expect_equal(val_label(tmp$Species, 1), "SETOSA")
  })
