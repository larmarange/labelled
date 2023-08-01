context("Test to_labelled()")


test_that("to_labelled.factor preserves variable label", {
  x <- factor(c(1, 1, 2))
  var_label(x) <- "test"
  expect_equal(var_label(to_labelled(x)), var_label(x))

  x <- factor(c("no", "yes", "no"))
  var_label(x) <- "test"
  expect_equal(
    var_label(to_labelled(x, labels = c("yes" = 1, "no" = 2))),
    var_label(x)
  )
})

test_that("to_labelled.factor preserves labelled character vectors", {
  s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
  expect_equal(s1, to_labelled(to_factor(s1), val_labels(s1)))
})

test_that("to_labelled.factor preserves labelled numerical vectors", {
  s2 <- labelled(c(1, 1, 2), c(Male = 1, Female = 2))
  expect_equal(s2, to_labelled(to_factor(s2), val_labels(s2)))
})

test_that("to_labelled.factor converts to NA factor levels not found in labels", { # nolint
  f <- factor(
    c("yes", "yes", "no", "no", "don't know", "no", "yes", "don't know")
  )
  expect_equal(
    to_labelled(f, c("yes" = 1, "no" = 2)),
    labelled(c(1, 1, 2, 2, NA, 2, 1, NA), c("yes" = 1, "no" = 2))
  )
})

test_that("to_labelled.factor accepts non continuous labels", {
  f <- factor(
    c("yes", "yes", "no", "no", "don't know", "no", "yes", "don't know")
  )
  expect_equal(
    to_labelled(f, c("yes" = 1, "no" = 2, "don't know" = 9)),
    labelled(
      c(1, 1, 2, 2, 9, 2, 1, 9),
      c("yes" = 1, "no" = 2, "don't know" = 9)
    )
  )
})

test_that("to_labelled.factor works with '[code] label' factors", {
  l <- labelled(
    c(1, 1, 2, 2, 9, 2, 1, 9),
    c("yes" = 1, "no" = 2, "don't know" = 9)
  )
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
  l <- labelled(
    c(1, 1, 2, 2, 9, 2, 1, 9),
    c("yes" = 1, "no" = 2, "don't know" = 9)
  )
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

# foreign_to_labelled -----------------------------------------------------


test_that("foreign_to_labelled works correctly", {
  utils::data("spss_file", package = "labelled")
  utils::data("dta_file", package = "labelled")

  tl_spss_list <- to_labelled(spss_file)
  expect_equal(
    val_labels(tl_spss_list),
    sapply(spss_file, function(x) attr(x, "value.labels", exact = TRUE))
  )
  expect_equal(
    var_label(tl_spss_list),
    as.list(attr(spss_file, "variable.labels", exact = TRUE))
  )
  miss_attr <- attr(spss_file, "missings", exact = TRUE)
  miss_list <- lapply(
    miss_attr,
    function(x) {
      if (x$type == "none") {
        return(NULL)
      } else {
        return(x$value)
      }
    }
  )
  expect_equal(sapply(tl_spss_list, na_values), miss_list)
  expect_true(
    all(
      which(sapply(tl_spss_list, function(x) any(is.na(x)))) == c(4, 5, 7, 10)
    )
  )

  tl_spss_df <- to_labelled(as.data.frame(spss_file, stringsAsFactors = FALSE))
  expect_equal(
    val_labels(tl_spss_df),
    sapply(spss_file, function(x) attr(x, "value.labels", exact = TRUE))
  )
  expect_true(all(sapply(var_label(tl_spss_df), is.null)))
  expect_true(all(sapply(sapply(tl_spss_df, na_values), is.null)))
  expect_true(all(sapply(sapply(tl_spss_df, na_range), is.null)))

  tl_dta_df <- to_labelled(dta_file)
  expect_equal(
    val_labels(tl_dta_df),
    sapply(dta_file, function(x) attr(x, "value.labels", exact = TRUE))
  )
  expect_equal(
    unname(unlist(var_label(tl_dta_df))),
    attr(dta_file, "var.labels", exact = TRUE)
  )
  expect_true(all(sapply(sapply(tl_dta_df, na_values), is.null)))
  expect_true(all(sapply(sapply(tl_dta_df, na_range), is.null)))
})

# memisc_to_labelled -----------------------------------------------------

test_that("memisc_to_labelled works correctly", {
  skip_if_not_installed("memisc")

  ds <- memisc::data.set(
    vote = sample(c(1, 2, 3, 8, 9, 97, 99), size = 300, replace = TRUE),
    region = sample(c(rep(1, 3), rep(2, 2), 3, 99), size = 300, replace = TRUE),
    income = exp(rnorm(300, sd = .7)) * 2000
  )

  memisc::description(ds$vote) <- "Vote intention"
  memisc::description(ds$region) <- "Region of residence"
  memisc::description(ds$income) <- "Household income"
  memisc::missing.values(ds$vote) <- c(97, 99)
  memisc::missing.values(ds$region) <- list(range = c(90, Inf))
  memisc::labels(ds$region) <- c(
    England               =  1,
    Scotland              =  2,
    Wales                 =  3,
    "Not applicable"      = 97,
    "Not asked in survey" = 99
  )
  memisc::labels(ds$vote) <- c(
    Conservatives         =  1,
    Labour                =  2,
    "Liberal Democrats"   =  3,
    "Don't know"          =  8,
    "Answer refused"      =  9,
    "Not applicable"      = 97,
    "Not asked in survey" = 99
  )

  tl_ds <- to_labelled(ds)

  desc <- data.frame(memisc::description(ds))
  var_label_ds <- desc[, 2]
  names(var_label_ds) <- desc[, 1]
  expect_identical(unlist(var_label(tl_ds)), var_label_ds)

  if (any(sapply(val_labels(tl_ds), function(x) !is.null(x)))) {
    val_labels_ds <- lapply(ds, function(x) memisc::labels(x))
    val_labels_ds <- lapply(ds, function(x) {
      vlabs <- memisc::labels(x)
      if (is.null(vlabs)) {
        return(NULL)
      }
      vals <- vlabs@values
      names(vals) <- vlabs@.Data
      return(vals)
    })
    expect_identical(val_labels(tl_ds), val_labels_ds)
  }
})

test_that("to_character works on data.frame", {
  df <- data.frame(
    x = labelled(c(1, 1, 2), c(yes = 1, no = 2)),
    y = c("a", "a", "b"),
    z = 1:3,
    stringsAsFactors = FALSE
  )

  df2 <- to_character(df)

  expect_true(is.character(df2$x))
  expect_equal(class(df2$y), class(df$y))
  expect_equal(class(df2$z), class(df$z))

  df3 <- to_character(df, labelled_only = FALSE)
  expect_true(is.character(df3$y))
  expect_true(is.character(df3$z))
})
