test_that("dictionary_to_variable_labels() works", {
  dic <- dplyr::tibble(
    variable = c("mpg", "vs", "am", "cyl"),
    label = c("miles / gallon", "Engine", "Transmission", NA),
    values = c(NA, "0:V-Shaped,1:straight", "0:automatic,1:manual", NA)
  )
  expect_no_error(
    l <- dic %>% dictionary_to_variable_labels()
  )
  expect_no_error(
    l <- dic %>%
      dictionary_to_variable_labels(names_from = variable, labels_from = label)
  )
  expect_no_error(
    m <-
      mtcars %>%
      set_variable_labels(.labels = l)
  )
  expect_equal(var_label(m$mpg), "miles / gallon")
})

test_that("dictionary_to_value_labels() works", {
  dic <- dplyr::tibble(
    variable = c("mpg", "vs", "am", "cyl"),
    label = c("miles / gallon", "Engine", "Transmission", NA),
    values = c(NA, "0:V-Shaped,1:straight", "0:automatic,1:manual", NA)
  )
  expect_no_error(
    vl <- dic %>%
      dictionary_to_value_labels(
        values_from = values,
        delim_entries = ",",
        delim_value_label = ":",
        data = mtcars
      )
  )
  expect_no_error(
    m <-
      mtcars %>%
      set_value_labels(.labels = vl)
  )
  expect_equal(
    val_labels(m$am),
    c(automatic = 0, manual = 1)
  )

  dic2 <- dplyr::tibble(
    variable = c("am", "am", "vs", "vs"),
    labels = c("0:automatic", "1:manual", "0:V-shaped", "1:straight")
  )
  expect_no_error(
    vl <-
      dic2 %>%
      dictionary_to_value_labels(
        delim_value_label = ":",
        data = mtcars
      )
  )
  expect_no_error(
    m <-
      mtcars %>%
      set_value_labels(.labels = vl)
  )
  expect_equal(
    val_labels(m$am),
    c(automatic = 0, manual = 1)
  )

  dic3 <- dplyr::tibble(
    code = c(0, 1, 0, 1),
    label = c("automatic", "manual", "V-shaped", "straight"),
    var = c("am", "am", "vs", "vs")
  )
  expect_no_error(
    vl <-
      dic3 %>%
      dictionary_to_value_labels(
        names_from = var,
        values_from = code,
        labels_from = label,
        data = mtcars
      )
  )
  expect_no_error(
    m <-
      mtcars %>%
      set_value_labels(.labels = vl)
  )
  expect_equal(
    val_labels(m$am),
    c(automatic = 0, manual = 1)
  )

  # with repeated delim_value_label
  dic4 <- dplyr::tibble(
    variable = c("mpg", "vs", "am", "cyl"),
    values = c(NA, "0,V-Shaped|1,straight,perfect", "0,automatic|1,manual", NA)
  )
  expect_no_error(
    vl <-
      dic4 %>%
      dictionary_to_value_labels(
        delim_entries = "|",
        delim_value_label = ","
      )
  )
  expect_equal(names(vl$vs)[2], "straight,perfect")
})

test_that("dictionary_to_value_labels() handles NA in names_from", {
  dic <- dplyr::tibble(
    variable = c("am", "vs", NA, "cyl"),
    values = c("0:automatic,1:manual", "0:V-shaped,1:straight", "0:foo,1:bar", NA)
  )
  expect_no_error(
    vl <- dic %>%
      dictionary_to_value_labels(
        values_from = values,
        delim_entries = ",",
        delim_value_label = ":"
      )
  )
  expect_true("am" %in% names(vl))
  expect_true("vs" %in% names(vl))
  expect_false(is.na("am") %in% names(vl))
})

test_that("dictionary_to_value_labels() handles NA in values_from", {
  dic <- dplyr::tibble(
    variable = c("am", "vs", "cyl"),
    values = c("0:automatic,1:manual", NA, "4:four,6:six,8:eight")
  )
  expect_no_error(
    vl <- dic %>%
      dictionary_to_value_labels(
        values_from = values,
        delim_entries = ",",
        delim_value_label = ":"
      )
  )
  expect_true("am" %in% names(vl))
  expect_true("cyl" %in% names(vl))
  expect_false("vs" %in% names(vl))
})

test_that("dictionary_to_value_labels() works without data argument", {
  dic <- dplyr::tibble(
    variable = c("am", "vs"),
    values = c("0:automatic,1:manual", "0:V-shaped,1:straight")
  )
  expect_no_error(
    vl <- dic %>%
      dictionary_to_value_labels(
        values_from = values,
        delim_entries = ",",
        delim_value_label = ":"
      )
  )
  expect_equal(vl$am, c(automatic = "0", manual = "1"))
  expect_equal(vl$vs, c(`V-shaped` = "0", straight = "1"))
})

test_that("dictionary_to_value_labels() ignores variables not in data", {
  dic <- dplyr::tibble(
    variable = c("am", "vs", "not_in_data"),
    values = c("0:automatic,1:manual", "0:V-shaped,1:straight", "1:foo,2:bar")
  )
  expect_no_error(
    vl <- dic %>%
      dictionary_to_value_labels(
        values_from = values,
        delim_entries = ",",
        delim_value_label = ":",
        data = mtcars
      )
  )
  expect_true("am" %in% names(vl))
  expect_true("vs" %in% names(vl))
  expect_true("not_in_data" %in% names(vl))
  # type coercion only happens for variables present in data
  expect_type(vl$am, "double")
  expect_type(vl$not_in_data, "character")
})

test_that("dictionary_to_variable_labels() handles NA in names_from", {
  dic <- dplyr::tibble(
    variable = c("mpg", NA, "am"),
    label = c("miles / gallon", "Engine", "Transmission")
  )
  expect_no_error(
    l <- dic %>% dictionary_to_variable_labels()
  )
  expect_true("mpg" %in% names(l))
  expect_true("am" %in% names(l))
  expect_equal(length(l), 2)
})
