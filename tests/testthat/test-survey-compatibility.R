test_that("labelled works with survey.design", {
  skip_on_cran()
  skip_if_not_installed("survey")

  ds <- survey::svydesign(~ 1, data = iris, weights = ~ 1)

  # variable labels
  expect_no_error(
    var_label(ds) <- list(Species = "type of iris")
  )
  expect_no_error(
    ds <- set_variable_labels(ds, Sepal.Length = "length of sepal")
  )
  expect_no_error(
    vl <- var_label(ds)
  )
  expect_equal(
    vl,
    list(
      Sepal.Length = "length of sepal",
      Sepal.Width = NULL,
      Petal.Length = NULL,
      Petal.Width = NULL,
      Species = "type of iris"
    )
  )

  # value labels
  expect_no_error(
    val_labels(ds) <- list(Sepal.Length = c("test 1" = 1))
  )
  expect_no_error(
    val_label(ds, v = 2) <- list(Sepal.Length = "test 2")
  )
  expect_no_error(
    ds <- add_value_labels(ds, Sepal.Length = c("test 3" = 3))
  )
  expect_no_error(
    vl <- get_value_labels(ds)
  )
  expect_equal(
    vl$Sepal.Length,
    c(`test 1` = 1, `test 2` = 2, `test 3` = 3)
  )
  expect_no_error(
    ds <- remove_value_labels(ds, Sepal.Length = 3)
  )
  expect_equal(
    val_labels(ds$variables$Sepal.Length),
    c(`test 1` = 1, `test 2` = 2)
  )
  expect_no_error(
    ds <- set_value_labels(ds, Sepal.Length = NULL)
  )
  expect_no_error(
    ds <- add_value_labels(ds, Sepal.Length = c(test = 3))
  )

  # update labels
  expect_no_error(
    update_variable_labels_with(ds, toupper)
  )
  expect_no_error(
    update_value_labels_with(ds, toupper)
  )

  # copy labels
  expect_no_error(
    copy_labels(ds, iris)
  )
  expect_no_error(
    copy_labels_from(ds, iris)
  )

  # drop_unused_value_labels
  expect_no_error(
    drop_unused_value_labels(ds)
  )

  # user_na
  expect_no_error(
    na_range(ds) <- list(Petal.Length = c(97, 98))
  )
  expect_no_error(
    na_values(ds) <- list(Petal.Length = 99)
  )
  expect_no_error(
    get_na_range(ds)
  )
  expect_no_error(
    get_na_values(ds)
  )
  expect_no_error(
    user_na_to_na(ds)
  )
  expect_no_error(
    user_na_to_tagged_na(ds)
  )

  # remove labels
  expect_no_error(
    remove_labels(ds)
  )
  expect_no_error(
    remove_var_label(ds)
  )
  expect_no_error(
    remove_val_labels(ds)
  )
  expect_no_error(
    remove_user_na(ds)
  )

  # tagged NAs
  expect_no_error(
    tagged_na_to_user_na(ds)
  )
  expect_no_error(
    tagged_na_to_regular_na(ds)
  )

  # to_character
  expect_no_error(
    to_character(ds)
  )

  # to_factor
  expect_no_error(
    to_factor(ds)
  )
  expect_no_error(
    unlabelled(ds)
  )

  # to_na
  expect_no_error(
    nolabel_to_na(ds)
  )
  expect_no_error(
    val_labels_to_na(ds)
  )
})

test_that("labelled works with svyrep.design", {
  skip_on_cran()
  skip_if_not_installed("survey")

  ds <- survey::svydesign(~ 1, data = iris, weights = ~ 1)
  ds <- survey::as.svrepdesign(ds)

  # variable labels
  expect_no_error(
    var_label(ds) <- list(Species = "type of iris")
  )
  expect_no_error(
    ds <- set_variable_labels(ds, Sepal.Length = "length of sepal")
  )
  expect_no_error(
    vl <- var_label(ds)
  )
  expect_equal(
    vl,
    list(
      Sepal.Length = "length of sepal",
      Sepal.Width = NULL,
      Petal.Length = NULL,
      Petal.Width = NULL,
      Species = "type of iris"
    )
  )

  # value labels
  expect_no_error(
    val_labels(ds) <- list(Sepal.Length = c("test 1" = 1))
  )
  expect_no_error(
    val_label(ds, v = 2) <- list(Sepal.Length = "test 2")
  )
  expect_no_error(
    ds <- add_value_labels(ds, Sepal.Length = c("test 3" = 3))
  )
  expect_no_error(
    vl <- get_value_labels(ds)
  )
  expect_equal(
    vl$Sepal.Length,
    c(`test 1` = 1, `test 2` = 2, `test 3` = 3)
  )
  expect_no_error(
    ds <- remove_value_labels(ds, Sepal.Length = 3)
  )
  expect_equal(
    val_labels(ds$variables$Sepal.Length),
    c(`test 1` = 1, `test 2` = 2)
  )
  expect_no_error(
    ds <- set_value_labels(ds, Sepal.Length = NULL)
  )
  expect_no_error(
    ds <- add_value_labels(ds, Sepal.Length = c(test = 3))
  )

  # update labels
  expect_no_error(
    update_variable_labels_with(ds, toupper)
  )
  expect_no_error(
    update_value_labels_with(ds, toupper)
  )

  # copy labels
  expect_no_error(
    copy_labels(ds, iris)
  )
  expect_no_error(
    copy_labels_from(ds, iris)
  )

  # drop_unused_value_labels
  expect_no_error(
    drop_unused_value_labels(ds)
  )

  # user_na
  expect_no_error(
    na_range(ds) <- list(Petal.Length = c(97, 98))
  )
  expect_no_error(
    na_values(ds) <- list(Petal.Length = 99)
  )
  expect_no_error(
    get_na_range(ds)
  )
  expect_no_error(
    get_na_values(ds)
  )
  expect_no_error(
    user_na_to_na(ds)
  )
  expect_no_error(
    user_na_to_tagged_na(ds)
  )

  # remove labels
  expect_no_error(
    remove_labels(ds)
  )
  expect_no_error(
    remove_var_label(ds)
  )
  expect_no_error(
    remove_val_labels(ds)
  )
  expect_no_error(
    remove_user_na(ds)
  )

  # tagged NAs
  expect_no_error(
    tagged_na_to_user_na(ds)
  )
  expect_no_error(
    tagged_na_to_regular_na(ds)
  )

  # to_character
  expect_no_error(
    to_character(ds)
  )

  # to_factor
  expect_no_error(
    to_factor(ds)
  )
  expect_no_error(
    unlabelled(ds)
  )

  # to_na
  expect_no_error(
    nolabel_to_na(ds)
  )
  expect_no_error(
    val_labels_to_na(ds)
  )
})
