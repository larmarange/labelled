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
})
