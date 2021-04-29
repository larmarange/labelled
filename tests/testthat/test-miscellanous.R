context("miscellanous")

# is_prefixed -------------------------------------------------------------

test_that("error with non factor argument", {
  x = 1:2
  expect_error(is_prefixed("x"))


})

test_that("is_prefixed() works properly", {
  x <- labelled(c(1, 2, 2, 2, 9, 1, 2, NA), c(yes = 1, no = 2, "don't know" = 9))
  tfx <- to_factor(x, levels = "prefixed")
  expect_true(is_prefixed(tfx))
  levels(tfx)[1] <- "not prefixed"
  expect_false(is_prefixed(tfx))
})

