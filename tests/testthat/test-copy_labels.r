test_that("copy_labels() copy variable / value labels and missing values", {
  x <- labelled(
    c(1, 1, 2),
    labels = c(Male = 1, Female = 2),
    label = "Assigned sex at birth"
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

  x <- dplyr::tibble(
    a = labelled(c(1, 1, 2), c(yes = 1, no = 2)),
    b = labelled_spss(1:3, c(top = 1, bottom = 2), na_values = 3L),
    c = c("a", "b", "c")
  ) %>%
    set_variable_labels(
      a = "variable a",
      b = "variable B",
      c = "third variable"
    )
  y <- dplyr::tibble(
    c = factor(c("a", "b", "c")),
    b = 2,
    a = 1:3,
    d = 9:7
  ) %>%
    copy_labels_from(x)
  expect_equal(var_label(x$a), var_label(y$a))
  expect_equal(val_labels(x$a), val_labels(y$a))
  expect_equal(na_range(x$a), na_range(y$a))
  expect_equal(na_values(x$a), na_values(y$a))
  expect_equal(var_label(x$b), var_label(y$b))
  expect_equal(val_labels(x$b), val_labels(y$b))
  expect_equal(na_range(x$b), na_range(y$b))
  expect_equal(na_values(x$b), na_values(y$b))
  expect_equal(var_label(x$c), var_label(y$c))
})

test_that("if 'from' is not a labelled vector, copy only variable label", {
  # regardless of the class of 'to'
  x <- 1:10
  var_label(x) <- "variable label"
  y <- 10:1 %>% copy_labels_from(x)
  expect_equal(var_label(x), var_label(y))

  x <- factor(1:10)
  var_label(x) <- "variable label"
  y <- 10:1 %>%
    as.character() %>%
    copy_labels_from(x)
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
    label = "Assigned sex at birth"
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

  # except if .strict = FALSE
  x <- labelled(
    c(1, 1, 2),
    labels = c(Male = 1, Female = 2),
    label = "Assigned sex at birth"
  )
  expect_no_error(
    copy_labels(x, c("1", "2"), .strict = FALSE)
  )
  x <- labelled_spss(
    1:10,
    labels = c(Good = 1, Bad = 8),
    na_values = c(9, 10),
    na_range = c(11, 19),
    label = "variable label"
  )
  expect_no_error(
    copy_labels(x, c("1", "2"), .strict = FALSE)
  )
})
