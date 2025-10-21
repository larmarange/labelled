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
})
