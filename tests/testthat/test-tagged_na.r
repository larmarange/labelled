context("Tests related to tagged NAs")

test_that("unique_tagged_na(), duplicated_tagged_na, order_tagged_na and sort_tagged_na work as expected", {
  x <- c(1, 2, tagged_na("a"), 1, tagged_na("z"), 2, tagged_na("a"), NA)

  expect_equal(
    unique_tagged_na(x) %>% format_tagged_na() %>% trimws(),
    c("1", "2", "NA(a)", "NA(z)", "NA")
  )
  expect_equal(
    unique_tagged_na(x, fromLast = TRUE) %>% format_tagged_na() %>% trimws(),
    c("1", "NA(z)", "2", "NA(a)", "NA")
  )

  expect_equal(
    duplicated_tagged_na(x),
    c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    duplicated_tagged_na(x, fromLast = TRUE),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )

  expect_equal(
    sort_tagged_na(x) %>% format_tagged_na() %>% trimws(),
    c("1", "1", "2", "2", "NA(a)", "NA(a)", "NA(z)", "NA")
  )
  expect_equal(
    sort_tagged_na(x, decreasing = TRUE) %>% format_tagged_na() %>% trimws(),
    c("2", "2", "1", "1", "NA(z)", "NA(a)", "NA(a)", "NA")
  )
  expect_equal(
    sort_tagged_na(x, na_decreasing = TRUE) %>% format_tagged_na() %>% trimws(),
    c("1", "1", "2", "2", "NA(z)", "NA(a)", "NA(a)", "NA")
  )
  expect_equal(
    sort_tagged_na(x, untagged_na_last = FALSE) %>% format_tagged_na() %>% trimws(),
    c("1", "1", "2", "2", "NA", "NA(a)", "NA(a)", "NA(z)")
  )
})

test_that("tagged_na_to_user_na() works as expected", {
  x <- c(1, 0, 1, tagged_na("r"), 0, tagged_na("d"), NA, tagged_na("d"), tagged_na("e"))
  val_labels(x) <- c(
    no = 0, yes = 1,
    "don't know" = tagged_na("d"),
    refusal = tagged_na("r")
  )
  expect_equal(
    tagged_na_to_user_na(x),
    labelled_spss(
      c(1, 0, 1, 4, 0, 2, NA, 2, 3),
      labels = c(no = 0, yes = 1, `don't know` = 2, refusal = 4, `NA(e)` = 3),
      na_range = c(2, 4)
    )
  )
  expect_equal(
    tagged_na_to_user_na(x, user_na_start = 8),
    labelled_spss(
      c(1, 0, 1, 10, 0, 8, NA, 8, 9),
      labels = c(no = 0, yes = 1, `don't know` = 8, refusal = 10, `NA(e)` = 9),
      na_range = c(8, 10)
    )
  )
})

test_that("tagged_na_to_regular_na() works as expected", {
  y <- c(1, 0, 1, tagged_na("r"), 0, tagged_na("d"))
  val_labels(y) <- c(
    no = 0, yes = 1,
    "don't know" = tagged_na("d"),
    refusal = tagged_na("r")
  )
  expect_false(any(tagged_na_to_regular_na(y) %>% is_tagged_na()))

  test <- rep(c(-99, -99, 3, 5, -1), 120)
  labelled::na_values(test) <- c(-99, -1)
  expect_warning(
    user_na_to_tagged_na(test),
    NA
  )
})
