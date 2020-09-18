context("Test copy_labels()")

test_that("copy_labels() copy variable / value labels and missing values", {
  x <- labelled(
    c(1, 1, 2),
    labels = c(Male = 1, Female = 2),
    label="Assigned sex at birth"
  )
  y <- copy_labels(x, 1:3)
  expect_equal(var_label(x), var_label(y))
  expect_equal(val_labels(x), val_labels(y))
  expect_equal(na_range(x), na_range(y))
  expect_equal(na_values(x), na_values(y))

  x <- labelled_spss(
    1:10,
    labels = c(Good = 1, Bad = 8),
    na_values = c(9, 10),
    na_range = c(11, 19),
    label = "variable label"
  )
  y <- 10:1 %>% copy_labels_from(x)
  expect_equal(var_label(x), var_label(y))
  expect_equal(val_labels(x), val_labels(y))
  expect_equal(na_range(x), na_range(y))
  expect_equal(na_values(x), na_values(y))
})

test_that("if 'from' is not a labelled vector, copy only variable label", {
  # regardless of the class of 'to'
  x <- 1:10
  var_label(x) <- "variable label"
  y <- 10:1 %>% copy_labels_from(x)
  expect_equal(var_label(x), var_label(y))

  x <- factor(1:10)
  var_label(x) <- "variable label"
  y <- 10:1 %>% as.character() %>% copy_labels_from(x)
  expect_equal(var_label(x), var_label(y))
})

test_that("copy_labels checks", {
  # do not work with a list
  expect_error(
    copy_labels(list(1, 2), 1:2)
  )
  expect_error(
    copy_labels(1:2, list(1, 2))
  )

  # if from is a data.frame, to should also be a data.frame
  expect_error(
    copy_labels(iris, 1:2)
  )
  expect_error(
    copy_labels(1:2, iris)
  )

  # if from is a labelled vector, to should have the same type
  x <- labelled(
    c(1, 1, 2),
    labels = c(Male = 1, Female = 2),
    label="Assigned sex at birth"
  )
  expect_error(
    copy_labels(x, c("1", "2"))
  )
  x <- labelled_spss(
    1:10,
    labels = c(Good = 1, Bad = 8),
    na_values = c(9, 10),
    na_range = c(11, 19),
    label = "variable label"
  )
  expect_error(
    copy_labels(x, c("1", "2"))
  )
})

