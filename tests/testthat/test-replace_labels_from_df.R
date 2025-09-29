test_that("replace_labels_from_df works with basic functionality", {
  # Create test data
  data <- data.frame(
    x = 1:3,
    y = 4:6,
    z = 7:9
  )

  # Create lookup data frame
  label_df <- data.frame(
    variable = c("x", "y"),
    label = c("X Variable", "Y Variable")
  )

  # Test basic functionality
  result <- replace_labels_from_df(data, label_df)

  expect_equal(var_label(result$x), "X Variable")
  expect_equal(var_label(result$y), "Y Variable")
  expect_null(var_label(result$z))
})

test_that("replace_labels_from_df respects overwrite parameter", {
  # Create test data with existing labels
  data <- data.frame(
    x = 1:3,
    y = 4:6
  )
  var_label(data$x) <- "Original X Label"
  var_label(data$y) <- "Original Y Label"

  # Create lookup data frame
  label_df <- data.frame(
    variable = c("x", "y"),
    label = c("New X Label", "New Y Label")
  )

  # Test with overwrite = TRUE (default)
  result1 <- replace_labels_from_df(data, label_df, overwrite = TRUE)
  expect_equal(var_label(result1$x), "New X Label")
  expect_equal(var_label(result1$y), "New Y Label")

  # Test with overwrite = FALSE
  result2 <- replace_labels_from_df(data, label_df, overwrite = FALSE)
  expect_equal(var_label(result2$x), "Original X Label")
  expect_equal(var_label(result2$y), "Original Y Label")
})

test_that("replace_labels_from_df handles missing variables gracefully", {
  # Create test data
  data <- data.frame(
    x = 1:3,
    y = 4:6
  )

  # Create lookup data frame with non-existent variable
  label_df <- data.frame(
    variable = c("x", "w", "z"),
    label = c("X Variable", "W Variable", "Z Variable")
  )

  # Should not error and should only apply existing variables
  expect_no_error({
    result <- replace_labels_from_df(data, label_df)
  })

  result <- replace_labels_from_df(data, label_df)
  expect_equal(var_label(result$x), "X Variable")
  expect_null(var_label(result$y))
})

test_that("replace_labels_from_df handles custom column names", {
  # Create test data
  data <- data.frame(
    x = 1:3,
    y = 4:6
  )

  # Create lookup data frame with custom column names
  label_df <- data.frame(
    var_name = c("x", "y"),
    var_label = c("X Variable", "Y Variable")
  )

  # Test with custom column names
  result <- replace_labels_from_df(
    data,
    label_df,
    label_df_variable_col = "var_name",
    label_df_label_col = "var_label"
  )

  expect_equal(var_label(result$x), "X Variable")
  expect_equal(var_label(result$y), "Y Variable")
})

test_that("replace_labels_from_df filters non-string labels", {
  # Create test data
  data <- data.frame(
    x = 1:3,
    y = 4:6,
    z = 7:9
  )

  # Create lookup data frame with mixed label types
  # Note: data.frame will convert 123 to "123", so let's be explicit
  label_df <- data.frame(
    variable = c("x", "y", "z"),
    label = I(list("X Variable", 123, NA)), # using I() to preserve types
    stringsAsFactors = FALSE
  )

  # Should only apply string labels
  result <- replace_labels_from_df(data, label_df)

  expect_equal(var_label(result$x), "X Variable")
  expect_null(var_label(result$y)) # numeric label ignored
  expect_null(var_label(result$z)) # NA label ignored
})

test_that("replace_labels_from_df validates inputs properly", {
  # Valid data for testing
  data <- data.frame(x = 1:3)
  label_df <- data.frame(variable = "x", label = "X Variable")

  # Test invalid data parameter
  expect_error(
    replace_labels_from_df("not a dataframe", label_df),
    "must be a data frame"
  )

  # Test invalid label_df parameter
  expect_error(
    replace_labels_from_df(data, "not a dataframe"),
    "must be a data frame"
  )

  # Test invalid column name parameters
  expect_error(
    replace_labels_from_df(data, label_df, label_df_variable_col = 123),
    "must be a single string"
  )

  expect_error(
    replace_labels_from_df(data, label_df, label_df_label_col = 123),
    "must be a single string"
  )

  # Test invalid overwrite parameter
  expect_error(
    replace_labels_from_df(data, label_df, overwrite = "yes"),
    "must be `TRUE` or `FALSE`"
  )

  # Test missing required columns
  bad_label_df <- data.frame(var = "x", lab = "X Variable")
  expect_error(
    replace_labels_from_df(data, bad_label_df),
    "Column 'variable' not found in label_df"
  )

  expect_error(
    replace_labels_from_df(data, bad_label_df, label_df_variable_col = "var"),
    "Column 'label' not found in label_df"
  )
})

test_that("replace_labels_from_df preserves data frame structure", {
  # Create tibble test data
  if (requireNamespace("dplyr", quietly = TRUE)) {
    data <- dplyr::tibble(
      x = 1:3,
      y = 4:6
    )

    label_df <- data.frame(
      variable = c("x", "y"),
      label = c("X Variable", "Y Variable")
    )

    result <- replace_labels_from_df(data, label_df)

    # Should preserve tibble class
    expect_s3_class(result, "tbl_df")
    expect_equal(var_label(result$x), "X Variable")
  }
})

test_that("replace_labels_from_df works with empty lookup data", {
  # Create test data
  data <- data.frame(
    x = 1:3,
    y = 4:6
  )
  var_label(data$x) <- "Original Label"

  # Empty lookup data frame
  label_df <- data.frame(
    variable = character(0),
    label = character(0)
  )

  # Should not change anything
  result <- replace_labels_from_df(data, label_df)

  expect_equal(var_label(result$x), "Original Label")
  expect_null(var_label(result$y))
})
