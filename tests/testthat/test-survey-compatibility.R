test_that("labelled works with survey design", {
  skip_on_cran()
  skip_if_not_installed("survey")

  ds <- survey::svydesign(~ 1, data = iris, weights = ~ 1)
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
})
