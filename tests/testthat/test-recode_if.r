context("Test recode_if()")

test_that("recode_if() works as expected", {
  x <- labelled(c(1, 2, 2, 9), c(yes = 1, no = 2))
  y <- x %>% recode_if(x == 9, NA)
  expect_equal(
    y,
    labelled(c(1, 2, 2, NA), c(yes = 1, no = 2))
  )

  y <- x %>% recode_if(1:4 < 3, 11:14)
  expect_equal(
    y,
    labelled(c(11, 12, 2, 9), c(yes = 1, no = 2))
  )

  x <- c("A", "B", "C")
  expect_equal(
    x %>% recode_if(c(TRUE, FALSE, NA), "Z"),
    c("Z", "B", "C")
  )
  expect_equal(
    x %>% recode_if(c(TRUE, FALSE, NA), 0),
    c("0", "B", "C")
  )
})

test_that("recode_if() preserve value and variable labels", {
  x <- labelled_spss(c(1,2,2,8,9), c(yes = 1, no = 2), na_values = 9)
  var_label(x) <- "variable label"
  y <- x %>% recode_if(unclass(x) == 8, NA)
  expect_equal(var_label(x), var_label(y))
  expect_equal(val_labels(x), val_labels(y))
  expect_equal(na_values(x), na_values(y))
  expect_equal(na_range(x), na_range(y))
})

test_that("recode_if() checks", {
  expect_error(
    1:3 %>% recode_if(c(TRUE, FALSE, NA), 9L),
    NA
  )
  expect_error(
    1:3 %>% recode_if(c(TRUE, FALSE, NA), 11:13),
    NA
  )

  expect_error(
    1:3 %>% recode_if(c(TRUE, FALSE, "NA"), 9)
  )
  expect_error(
    1:3 %>% recode_if(c(TRUE, FALSE), 9)
  )
  expect_error(
    1:3 %>% recode_if(c(TRUE, FALSE, NA), 10:11)
  )

  expect_warning(
    1:3 %>% recode_if(c(TRUE, FALSE, NA), "char")
  )
  expect_warning(
    1:3 %>% recode_if(c(TRUE, FALSE, NA), 9)
  )
})

