test_that("look_for works correctly", {
  df <- data.frame(
    1:3,
    letters[1:3],
    fix.empty.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_error(look_for(df))
  expect_error(look_for(unname(df)))

  df <- data.frame(num = 1:3, ch = letters[1:3], stringsAsFactors = FALSE)
  res <- look_for(df, "e")
  capture.output(print(res))
  expect_true(nrow(res) == 0)
})

test_that("look_for works with a single keyword.", {
  expect_equal(
    look_for(iris, "sep")$variable,
    c("Sepal.Length", "Sepal.Width")
  )

  lfi <- look_for(iris, "s")
  expect_equal(
    lfi$variable,
    c("Sepal.Length", "Sepal.Width", "Species")
  )
  expect_equal(
    lfi$levels,
    list(
      "Sepal.Length" = NULL,
      "Sepal.Width" = NULL,
      "Species" = levels(iris$Species)
    )
  )

  expect_equal(lfi$variable, names(iris)[lfi$pos])

  expect_no_error(
    look_for(iris, "petal") %>% dplyr::select(pos) %>% print()
  )
})

test_that("look_for works with no single keyword.", {
  expect_equal(
    look_for(iris, details = TRUE)$variable,
    names(iris)
  )
})

test_that("look_for works with a regular expression", {
  lfi <- look_for(iris, "s")
  expect_identical(look_for(iris, "sepal|species"), lfi)

  lfi <- look_for(iris, "s$")
  expect_identical(
    lfi$levels[[lfi$variable]],
    levels(iris$Species)
  )
})


test_that("look_for works with several keywords", {
  expect_equal(
    look_for(iris, details = "none", "s", "w")$variable,
    c("Sepal.Length", "Sepal.Width", "Petal.Width", "Species")
  )
  expect_equal(
    look_for(iris, "Pet", "sp", "width", ignore.case = FALSE)$variable,
    c("Petal.Length", "Petal.Width")
  )
})


test_that(" look_for with different details parameter values", {
  expect_false("levels" %in% names(look_for(iris, details = "none")))
  expect_false("range" %in% names(look_for(iris, "Sep")))
  expect_equal(
    look_for(iris, details = TRUE, "sep")$range,
    list(
      Sepal.Length = range(iris$Sepal.Length),
      Sepal.Width = range(iris$Sepal.Width)
    )
  )
})


test_that(" convert_list_columns_to_character works correctly", {
  lfi_conv <- look_for(iris, "spe", details = TRUE) %>%
    convert_list_columns_to_character()
  expect_equal(
    unname(lfi_conv$levels),
    paste(levels(iris$Species), collapse = "; ")
  )

  lfi_conv <- look_for(iris, "al", details = TRUE) %>%
    convert_list_columns_to_character()
  expect_identical(
    lfi_conv$range,
    sapply(
      lapply(iris[, lfi_conv$variable], range),
      function(x) paste(x, collapse = " - ")
    )
  )

  lfi_conv <- look_for(iris, "sep") %>% convert_list_columns_to_character()
  expect_true(all(lfi_conv$levels == c("", "")))
  expect_true(all(lfi_conv$value_labels == c("", "")))
})

test_that(" look_for_and_select works correctly", {
  expect_equal(
    names(look_for_and_select(iris, "sep")),
    c("Sepal.Length", "Sepal.Width")
  )
})

test_that(" print.look_for works correctly", {
  pp <- print(look_for(iris))
  expect_equal(
    pp$variable[nchar(pp$variable) != 0],
    names(iris)
  )
  expect_equal(
    pp$values[nchar(pp$values) != 0],
    levels(iris$Species)
  )
})

test_that(" lookfor_to_long_format works correctly", {
  lf2lf <- look_for(iris) %>% lookfor_to_long_format()
  expect_equal(
    lf2lf$levels[lf2lf$variable == "Species"],
    levels(iris$Species)
  )

  expect_equal(
    iris,
    lookfor_to_long_format(iris)
  )

  expect_true(all(is.na(lf2lf$levels[lf2lf$variable != "Species"])))
})

test_that("look_for get var_label", {
  df <- data.frame(col1 = 1:2, col2 = 3:4, stringsAsFactors = FALSE)
  expect_equal(nrow(look_for(df, "lb")), 0)
  var_label(df) <- c("lb1", "lb2")
  lfd <- look_for(df, "lb")

  expect_equal(
    lfd$variable,
    names(df)
  )
  expect_equal(
    unname(lfd$label),
    c("lb1", "lb2")
  )
})

test_that("look_for works with factor levels and value labels", {
  res <- iris %>% look_for("vers", details = "none")
  expect_equal(res$variable, "Species")

  res <- iris %>% look_for("vers", details = "none", values = FALSE)
  expect_equal(nrow(res), 0)

  df <- iris
  df$Species <- to_labelled(df$Species)

  res <- df %>% look_for("vers", details = "none")
  expect_equal(res$variable, "Species")

  res <- df %>% look_for("vers", details = "none", values = FALSE)
  expect_equal(nrow(res), 0)
})

test_that("to_gt() does not produce errors", {
  skip_on_cran()
  skip_if_not_installed("gt")
  skip_if_not_installed("purrr")

  expect_no_error(
    tbl <- iris %>% look_for() %>% to_gt()
  )
  expect_s3_class(tbl, "gt_tbl")
  expect_no_error(
    tbl <- iris %>% look_for(details = FALSE) %>% to_gt()
  )
  expect_no_error(
    tbl <- iris %>% look_for(details = TRUE) %>% to_gt()
  )
  expect_no_error(
    tbl <- iris %>% look_for("no_data") %>% to_gt()
  )

  d <- dplyr::tibble(
    region = labelled_spss(
      c(1, 2, 1, 9, 2, 3),
      c(north = 1, south = 2, center = 3, missing = 9),
      na_values = 9,
      na_range = c(90, 99),
      label = "Region of the respondent"
    ),
    sex = labelled(
      c("f", "f", "m", "m", "m", "f"),
      c(female = "f", male = "m"),
      label = "Sex of the respondent"
    )
  )
  expect_no_error(
    tbl <- d %>% look_for() %>% to_gt()
  )
  expect_no_error(
    tbl <- d %>% look_for(details = FALSE) %>% to_gt()
  )
  expect_no_error(
    tbl <- d %>% look_for(details = TRUE) %>% to_gt()
  )
  expect_no_error(
    tbl <- d %>% look_for() %>% to_gt(column_labels = NULL)
  )
  expect_no_error(
    tbl <- d %>%
      look_for() %>%
      to_gt(column_labels = list(pos = "POSITION"))
  )
})
