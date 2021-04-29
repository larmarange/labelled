context("miscellanous")

# is_prefixed -------------------------------------------------------------

test_that("error with non factor argument", {
  x = 1:2
  expect_error(is_prefixed("x"))


})

test_that("is_prefixed works properly", {

  x <- haven::labelled(c(c(1,2,3,5,4), NA, 99), c(t1 = 1, t2 = 2, t5 = 5, Missing = 99))

  tfx <- to_factor(x, levels = "prefixed")
  expect_true(is_prefixed(tfx))
  levels(tfx)[1] <- "t1"
  expect_false(is_prefixed(tfx))
})

