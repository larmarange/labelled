context("Test to_labelled()")


test_that("to_labelled.factor preserves variable label", {
  x <- factor(c(1, 1, 2))
  var_label(x) <- "test"
  expect_equal(var_label(to_labelled(x)), var_label(x))

  x <- factor(c("no", "yes", "no"))
  var_label(x) <- "test"
  expect_equal(var_label(to_labelled(x, labels = c("yes" = 1, "no" = 2))), var_label(x))
})

test_that("to_labelled.factor preserves labelled character vectors", {
  s1 <- labelled(c('M', 'M', 'F'), c(Male = 'M', Female = 'F'))
  expect_equal(s1, to_labelled(to_factor(s1), val_labels(s1)))
})

test_that("to_labelled.factor preserves labelled numerical vectors", {
  s2 <- labelled(c(1, 1, 2), c(Male = 1, Female = 2))
  expect_equal(s2, to_labelled(to_factor(s2), val_labels(s2)))
})

test_that("to_labelled.factor converts to NA factor levels not found in labels", {
  f <- factor(c("yes", "yes", "no", "no", "don't know", "no", "yes", "don't know"))
  expect_equal(
    to_labelled(f, c("yes" = 1, "no" = 2)),
    labelled(c(1, 1, 2, 2, NA, 2, 1, NA), c("yes" = 1, "no" = 2))
  )
})

test_that("to_labelled.factor accepts non continuous labels", {
  f <- factor(c("yes", "yes", "no", "no", "don't know", "no", "yes", "don't know"))
  expect_equal(
    to_labelled(f, c("yes" = 1, "no" = 2, "don't know" = 9)),
    labelled(c(1, 1, 2, 2, 9, 2, 1, 9), c("yes" = 1, "no" = 2, "don't know" = 9))
  )
})
