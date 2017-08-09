context("Labelled")

test_that("labelled return an object of class labelled",{
  x <- labelled(c(1,2,3), c(yes = 1, maybe = 2, no = 3))
  expect_that(is.labelled(x), is_true())
  expect_equal(class(x), "labelled")
})

test_that("x must be numeric or character", {
  expect_error(labelled(TRUE))
})

test_that("x and labels must be compatible", {
  expect_error(labelled(1, "a"))
  expect_error(labelled(1, c(female = 2L, male = 1L)), NA)
  expect_error(labelled(1L, c(female = 2, male = 1)), NA)
})

test_that("labels must have names", {
  expect_error(labelled(1, 1))
})

test_that("labelled preserves variable label", {
  x <- 1:3
  var_label(x) <- "test"
  x <- labelled(x, c(yes = 1))
  expect_equal(attr(x, "label", exact = TRUE), "test")
})

# var_labels and var_label ------------------------------------------------

test_that("val_labels preserves variable label", {
  x <- 1:3
  var_label(x) <- "test"
  val_labels(x) <- c(yes = 1, no = 2)
  expect_equal(attr(x, "label", exact = TRUE), "test")

  val_labels(x) <- NULL
  expect_equal(attr(x, "label", exact = TRUE), "test")
})

test_that("val_label preserves variable label", {
  x <- 1:3
  var_label(x) <- "test"
  val_label(x, 1) <- "yes"
  expect_equal(attr(x, "label", exact = TRUE), "test")

  val_label(x, 1) <- NULL
  expect_equal(attr(x, "label", exact = TRUE), "test")
})

test_that("val_labels and val_label preserves spss missing values", {
  x <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10), na_range = c(11, Inf))
  val_labels(x) <- c(yes = 1, no = 3)
  val_label(x, 2) <- "maybe"
  expect_true(inherits(x, "labelled"))
  expect_true(inherits(x, "labelled_spss"))
  expect_equal(attr(x, "na_values"), c(9, 10))
  expect_equal(attr(x, "na_range"), c(11, Inf))

  val_label(x, 2) <- "maybe"
  expect_true(inherits(x, "labelled"))
  expect_true(inherits(x, "labelled_spss"))
  expect_equal(attr(x, "na_values"), c(9, 10))
  expect_equal(attr(x, "na_range"), c(11, Inf))

  expect_equal(attr(x, "labels", exact = TRUE), c(yes = 1, no = 3, maybe = 2))
})

test_that("value labels can't be removed if missing values are defined", {
  x <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
  expect_error(`val_labels<-`(x, NULL))

  x <- labelled_spss(1:10, c(Good = 1), na_range = c(9, 20))
  expect_error(`val_label<-`(x, 1, NULL))
})

# remove_labels --------------------------------------------------------------

test_that("remove_labels strips labelled attributes", {
  var <- labelled(c(1L, 98L, 99L),  c(not_answered = 98L, not_applicable = 99L))
  exp <- c(1L,98L,99L)
  expect_equal(remove_labels(var), exp)
})

test_that("remove_labels returns variables not of class('labelled') unmodified", {
  var <- c(1L, 98L, 99L)
  expect_equal(remove_labels(var), var)
})

# to_factor --------------------------------------------------------------------

test_that("to_factor preserves variable label", {
  x <- labelled(c(1, 1, 2), c(yes = 1, no = 2))
  var_label(x) <- "yes/no"
  expect_equal(var_label(to_factor(x)), var_label(x))
})


# to_character --------------------------------------------------------------------

test_that("to_character produce an appropriate character vector", {
  x <- labelled(c(1, 1, 2), c(yes = 1, no = 2))
  expect_equal(class(to_character(x)), "character")
  expect_equal(to_character(x), c("yes", "yes", "no"))
})


test_that("to_character preserves variable label", {
  x <- labelled(c(1, 1, 2), c(yes = 1, no = 2))
  var_label(x) <- "yes/no"
  expect_equal(var_label(to_character(x)), var_label(x))
})

# to_labelled.factor ---------------------------------------------------------

test_that("to_labelled.factor preserves variable label", {
  x <- factor(c(1, 1, 2))
  var_label(x) <- "test"
  expect_equal(var_label(to_labelled(x)), var_label(x))
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

# set_value_labels and add_value_labels ----------------------------------------------

test_that("set_value_labels replaces all value labels", {
  df <- data.frame(s1 = c("M", "M", "F"), s2 = c(1, 1, 2), stringsAsFactors = FALSE)
  df <- set_value_labels(df, s1 = c(Male = "M", Female = "F"), s2 = c(Yes = 1, No = 2))
  expect_equal(val_labels(df$s1), c(Male = "M", Female = "F"))
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2))
  df <- set_value_labels(df, s2 = c(Yes = 1, Unknown = 9))
  expect_equal(val_labels(df$s2), c(Yes = 1, Unknown = 9))
})

test_that("add_value_labels and remove_value_labels updates the list of value labels", {
  df <- data.frame(s1 = c("M", "M", "F"), s2 = c(1, 1, 2), stringsAsFactors = FALSE)
  df <- set_value_labels(df, s1 = c(Male = "M", Female = "F"), s2 = c(Yesss = 1, No = 2))
  df <- add_value_labels(df, s2 = c(Yes = 1, Unknown = 9))
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2, Unknown = 9))
  df <- remove_value_labels(df, s2 = 9)
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2))
})

# set_variable_labels  --------------------------------------------------------------

test_that("set_variable_labels updates variable labels", {
  df <- data.frame(s1 = c("M", "M", "F"), s2 = c(1, 1, 2), stringsAsFactors = FALSE)
  df <- set_variable_labels(df, s1 = "Sex", s2 = "Question")
  expect_equal(var_label(df$s1), "Sex")
  df <- set_variable_labels(df, s2 = NULL)
  expect_null(var_label(df$s2))
})

# missing values -------------------------------------------------------------------

test_that("it is not possible to define missing values if no value labels were defined", {
  expect_error(`na_values<-`(1:3, 9))
  expect_error(`na_range<-`(1:3, c(9, Inf)))
})
