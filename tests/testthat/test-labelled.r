library(labelled)
context("labelled")

test_that("labelled return an object of class labelled",{
  x <- labelled(c(1,2,3), c(yes = 1, maybe = 2, no = 3))

  expect_that(is.labelled(x), is_true())
  expect_equal(class(x), "labelled")
})

