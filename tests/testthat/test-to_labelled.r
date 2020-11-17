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

test_that("to_labelled.factor works with '[code] label' factors", {
  l <- labelled(c(1, 1, 2, 2, 9, 2, 1, 9), c("yes" = 1, "no" = 2, "don't know" = 9))
  expect_equal(
    to_factor(l, levels = "p") %>% to_labelled(),
    l
  )

  l <- labelled(
    c("M", "M", "F", "X", "N/A"),
    c(Male = "M", Female = "F", Refused = "X", "Not applicable" = "N/A")
  )
  expect_equal(
    to_factor(l, levels = "p") %>% to_labelled(),
    l
  )

  # if labels is provided apply normal rule
  l <- labelled(c(1, 1, 2, 2, 9, 2, 1, 9), c("yes" = 1, "no" = 2, "don't know" = 9))
  f <- to_factor(l, levels = "p")
  x <- f %>% to_labelled(labels = c("[1] yes" = 123, "[2] no" = 456))
  expect_equivalent(
    unclass(x),
    c(123, 123, 456, 456, NA, 456, 123, NA)
  )

  # should not be applied if duplicates in code
  f <- factor(c("[1] yes", "[2] no", "[1] don't know"))
  expect_warning(l <- to_labelled(f))
  expect_warning(l <- to_labelled(f, .quiet = TRUE), NA)
  expect_identical(
    names(val_labels(l)),
    levels(f)
  )

  # check potential duplicates in numerical codes
  f <- factor(c("[1] yes", "[1.0] no", "[01] don't know"))
  expect_warning(to_labelled(f))
  expect_warning(to_labelled(f, .quiet = TRUE), NA)
  expect_true(is.character(to_labelled(f, .quiet = TRUE)))
})
