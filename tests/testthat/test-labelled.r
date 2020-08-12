context("Labelled")

# var_label --------------------------------------------------------------

test_that("var_label works properly", {
  x <- 1:3
  var_label(x) <- "value"
  expect_equal(attr(x, "label"), "value")
  expect_equal(var_label(x), "value")
  var_label(x) <- NULL
  expect_null(attr(x, "label"))
  expect_null(var_label(x))
})

test_that("var_label works on data.frame", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  var_label(df$x) <- "var x"
  expect_equal(var_label(df$x), "var x")
  expect_equal(var_label(df), list(x = "var x", y = NULL))
  var_label(df) <- list(y = "YY", x = "XX")
  expect_equal(var_label(df), list(x = "XX", y = "YY"))
  var_label(df) <- NULL
  expect_equal(var_label(df), list(x = NULL, y = NULL))
  var_label(df) <- c("var1", "var2")
  expect_equal(var_label(df), list(x = "var1", y = "var2"))
  df <- set_variable_labels(df, x = "XX", .labels = "other")
  expect_equal(var_label(df), list(x = "XX", y = "other"))
  df <- set_variable_labels(df, .labels = c("var1", "var2"))
  expect_equal(var_label(df), list(x = "var1", y = "var2"))
})


test_that("var_label produce appropriate errors", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  expect_error(var_label(df) <- c("var1", "var2", "var3"))
  expect_error(var_label(df) <- list(x = "xx", z = "zz"))
})

test_that("var_label preserved data.frame type", {
  tb <- dplyr::tibble(x = 1:3, y = c("a", "b", "c"))
  before <- class(tb)
  var_label(tb$x) <- "var x"
  var_label(tb) <- list(y = "YY", x = "XX")
  after <- class(tb)
  expect_equal(before, after)
})

# labelled --------------------------------------------------------------

test_that("labelled return an object of class haven_labelled",{
  x <- labelled(c(1,2,3), c(yes = 1, maybe = 2, no = 3))
  expect_true(is.labelled(x))
  expect_s3_class(x, "haven_labelled")
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


# val_labels and val_label ------------------------------------------------

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
  expect_true(inherits(x, "haven_labelled"))
  expect_true(inherits(x, "haven_labelled_spss"))
  expect_equal(attr(x, "na_values"), c(9, 10))
  expect_equal(attr(x, "na_range"), c(11, Inf))

  val_label(x, 2) <- "maybe"
  expect_true(inherits(x, "haven_labelled"))
  expect_true(inherits(x, "haven_labelled_spss"))
  expect_equal(attr(x, "na_values"), c(9, 10))
  expect_equal(attr(x, "na_range"), c(11, Inf))

  expect_equal(attr(x, "labels", exact = TRUE), c(yes = 1, no = 3, maybe = 2))
})

test_that("value labels can be removed if missing values are defined", {
  x <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
  val_labels(x) <- NULL
  expect_null(val_labels(x))

  x <- labelled_spss(1:10, c(Good = 1), na_range = c(9, 20))
  val_labels(x) <- NULL
  expect_null(val_labels(x))
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

test_that("strict option of to_factor works correctly", {
  v <- labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2))
  expect_s3_class(to_factor(v, strict = FALSE), "factor")
  expect_s3_class(to_factor(v, strict = TRUE), "haven_labelled")
  expect_equal(class(to_factor(v, strict = TRUE, unclass = TRUE)), "numeric")
})

test_that("to_factor works on data.frame", {
  df <- data.frame(
    x = labelled(c(1, 1, 2), c(yes = 1, no = 2)),
    y = c("a", "a", "b"),
    z = 1:3,
    stringsAsFactors = FALSE
  )

  df2 <- to_factor(df)

  expect_true(is.factor(df2$x))
  expect_equal(class(df2$y), class(df$y))
  expect_equal(class(df2$z), class(df$z))
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

test_that("it is possible to define missing values if no value labels were defined", {
  x <- c(1, 2, 2, 9)
  na_values(x) <- 9
  expect_equal(na_values(x), 9)

  x <- c(1, 2, 2, 9)
  na_range(x) <- 9:10
  expect_equal(na_range(x), 9:10)
})

test_that("na_values and na_range keep variable label", {
  vl <- "variable label"
  x <- 1:9
  var_label(x) <- vl
  na_values(x) <- 8
  na_range(x) <- c(9, Inf)
  expect_equal(var_label(x), vl)
})

# recode (dplyr) -------------------------------------------------------------------
test_that("dplyr::recode could be applied to numeric labelled vector", {
  x <- dplyr::recode(labelled(1:3, c(yes = 1, no = 2)), `3` = 2L)
  expect_equal(x, labelled(c(1L, 2L, 2L), c(yes = 1, no = 2)))
})

test_that("dplyr::recode could be applied to character labelled vector", {
  x <- dplyr::recode(labelled(c("a", "b", "c"), c(yes = "a", no = "b")), c = "b")
  expect_equal(x, labelled(c("a", "b", "b"), c(yes = "a", no = "b")))
})

# update_labelled ----------------------------------------

test_that("update_labelled update previous haven's labelled objects but not Hmisc's labelled objects", {
  vhaven <- structure(1:4, label = "label", labels = c(No = 1, Yes = 2), class = "labelled")
  vHmisc <- structure(1:4, label = "label", class = "labelled")

  expect_s3_class(update_labelled(vhaven), "haven_labelled")
  expect_s3_class(update_labelled(vHmisc), "labelled")

  df <- dplyr::tibble(vhaven, vHmisc)
  expect_s3_class(update_labelled(df)$vhaven, "haven_labelled")
  expect_s3_class(update_labelled(df)$vHmisc, "labelled")
})

test_that("update_labelled update to haven_labelled_spss if there are na values", {
  v1 <- structure(1:4, label = "label", labels = c(No = 1, Yes = 2),
                  na_values = c(8, 9), class = c("labelled_spss", "labelled"))
  v2 <- structure(1:4, label = "label", labels = c(No = 1, Yes = 2),
                  na_range = c(8, 9), class = c("labelled_spss", "labelled"))

  expect_s3_class(update_labelled(v1), "haven_labelled_spss")
  expect_s3_class(update_labelled(v1), "haven_labelled_spss")
})

test_that("update_labelled preserve variable and value labels", {
  v <- structure(1:4, label = "variable label", labels = c(No = 1, Yes = 2), class = "labelled")

  expect_equal(var_label(update_labelled(v)), "variable label")
  expect_equal(val_labels(update_labelled(v)), c(No = 1, Yes = 2))
})

test_that("update_labelled do nothing if it's not a labelled vector", {
  x <- 1:10
  expect_equal(update_labelled(x), x)
})

# remove_attributes --------------------------------------------------------------

test_that("remove_attributes does not transform characters into factors", {
  d <- data.frame(
    ch = structure(letters[1:2], some_attribute=TRUE),
    stringsAsFactors = FALSE
  )
  d <- remove_attributes(d, "some_attribute")
  expect_true(is.character(d$ch))
})



# unlabelled --------------------------------------------------------------------

test_that("unlabelled works correctly", {
  df <- data.frame(
    a = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2)),
    b = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2, DK = 3)),
    c = labelled(c("a", "a", "b", "c"), labels = c(No = "a", Yes = "b"))
  )

  df <- unlabelled(df)
  expect_equal(class(df$a), "numeric")
  expect_s3_class(df$b, "factor")
  expect_equal(class(df$c), "character")

  v <- labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2, DK = 3))
  expect_s3_class(unlabelled(v), "factor")

  v <- labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2))
  expect_false(inherits(unlabelled(v), "haven_labelled"))
})

# remove_label ------------------------------------------


test_that("remove_label works correctly", {
  x <- c(1, 2, 2, 9)
  na_values(x) <- 9
  val_labels(x) <- c(yes = 1, no = 2)
  var_label(x) <- "A test variable"

  expect_false(inherits(remove_labels(x), "haven_labelled"))
  expect_null(var_label(remove_labels(x)))
  expect_equal(
    var_label(remove_labels(x, keep_var_label = TRUE)),
    var_label(x)
  )
})

# recode --------------------------------------------------------------

test_that("dplyr::recode works properly with labelled vectors", {
  x <- labelled(1:3, c(yes = 1, no = 2))

  r <- dplyr::recode(x, `3` = 2L)
  expect_equal(r, labelled(c(1L, 2L, 2L), val_labels(x)))

  r <- dplyr::recode(x, `3` = 2L, .keep_value_labels = FALSE)
  expect_equal(r, c(1L, 2L, 2L))

  expect_warning(dplyr::recode(x, `3` = "a", .default = "b"))

  x <- labelled(1:4, c(a = 1, b = 2, c = 3, d = 4))

  r <- dplyr::recode(x, `1` = 1L, `2` = 1L, `3` = 2L, `4` = 2L, .combine_value_labels = TRUE)
  expect_equal(val_labels(r), c("a / b" = 1L, "c / d" = 2L))

  r <- dplyr::recode(x, `2` = 1L, `4` = 3L, .combine_value_labels = TRUE)
  expect_equal(val_labels(r), c("a / b" = 1L, "c / d" = 3L))

  r <- dplyr::recode(x, `2` = 1L, `4` = 3L, .combine_value_labels = TRUE, .sep = " or ")
  expect_equal(val_labels(r), c("a or b" = 1L, "c or d" = 3L))

  y <- labelled(1:4, c(a = 1))
  r <- dplyr::recode(y, `2` = 1L, `4` = 3L, .combine_value_labels = TRUE)
  expect_equal(val_labels(r), c(a = 1L))
})

# tidy dots --------------------------------------------------------------
test_that("functions with dots accept tidy evaluation (`!!!` operator)", {
  df <- data.frame(s1 = c("M", "M", "F"), s2 = c(1, 1, 2), stringsAsFactors = FALSE)
  variable_list <- list(s1 = "Sex", s2 = "Question")
  df <- set_variable_labels(df, !!!variable_list)
  expect_equal(var_label(df$s1), "Sex")
  expect_equal(var_label(df$s2), "Question")

  df <- data.frame(s1 = c("M", "M", "F"), s2 = c(1, 1, 2), stringsAsFactors = FALSE)
  labels_list <- list(s1 = c(Male = "M", Female = "F"), s2 = c(Yes = 1, No = 2))
  df <- set_value_labels(df, !!!labels_list)
  expect_equal(val_labels(df$s1), c(Male = "M", Female = "F"))
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2))

  df <- data.frame(s1 = c("M", "M", "F"), s2 = c(1, 1, 2), stringsAsFactors = FALSE)
  df <- set_value_labels(df, s1 = c(Male = "M", Female = "F"), s2 = c(Yesss = 1, No = 2))
  added_values_list <- list(s2 = c(Yes = 1, Unknown = 9))
  df <- add_value_labels(df, !!!added_values_list)
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2, Unknown = 9))
  removed_values_list <- list(s2 = 9)
  df <- remove_value_labels(df, !!!removed_values_list)
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2))
})
