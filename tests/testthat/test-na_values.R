# na_values --------------------------------------------------------------------

test_that("na_values works with data.frame", {
  xhs <- haven::labelled_spss(
    c(1, 2, 3, NA, 99),
    c(t1 = 1, t2 = 2, Missing = 99),
    na_value = 99,
    label = "variable label"
  )
  y <- c(1:4, NA)
  df <- data.frame(xhs = xhs, y = y, stringsAsFactors = FALSE)
  res <- list(xhs = 99, y = NULL)
  expect_equal(na_values(df), res)
})

# na_range --------------------------------------------------------------------

test_that("na_range works with data.frame", {
  xhs <- haven::labelled_spss(
    c(1, 2, 3, NA, 99),
    c(t1 = 1, t2 = 2, Missing = 99),
    na_value = 99,
    na_range = c(99, Inf),
    label = "variable label"
  )
  y <- c(1:4, NA)
  df <- data.frame(xhs = xhs, y = y, stringsAsFactors = FALSE)
  res <- list(xhs = c(99, Inf), y = NULL)
  expect_equal(na_range(df), res)
})

# user_na_to_na ----------------------------------------------------------------

test_that("user_na_to_na works with data.frame", {
  xhs <- haven::labelled_spss(
    c(c(1, 2, 3), NA, 99),
    c(t1 = 1, t2 = 2, Missing = 99),
    na_value = 99,
    na_range = c(99, Inf),
    label = "variable label"
  )
  y <- c(1:4, NA)
  df <- data.frame(xhs = xhs, y = y, stringsAsFactors = FALSE)

  una_df <- user_na_to_na(df)
  expect_equal(df$y, y)
  expect_null(na_values(una_df$x))
  expect_null(na_range(una_df$x))
})

# set_na_values ----------------------------------------------------------------

test_that("set_na_values works correctly", {
  df <- dplyr::tibble(s1 = c("M", "M", "F", "F"), s2 = c(1, 1, 2, 9))
  svdf <- set_value_labels(df, s2 = c(yes = 1, no = 2))

  sna_svdf <- set_na_values(svdf, s2 = 9)
  expect_equal(which(is.na(sna_svdf$s2)), 4L)

  expect_error(set_na_values(svdf, s4 = 8, s2 = 9))
  sna_svdfF <- set_na_values(svdf, s4 = 8, s2 = 9, .strict = FALSE)
  expect_equal(sna_svdf, sna_svdfF)
  expect_error(set_na_values(svdf, .values = list(s2 = 9, s4 = 3)))
  expect_error(set_na_values(svdf, .values = c(s2 = 9)))

  snu_svdf <- set_na_values(sna_svdfF, s2 = NULL)
  expect_equal(snu_svdf, svdf)
  snu_svdf <- set_na_values(sna_svdf, s2 = NULL)
  expect_equal(snu_svdf, svdf)

  df <- dplyr::tibble(s1 = c(2, 4, 7, 9), s2 = c(1, 1, 2, 9))
  svdf <- set_value_labels(df, s2 = c(yes = 1, no = 2))
  sna_svdf <- set_na_values(svdf, .values = 9L)
  expect_equal(na_values(sna_svdf), list(s1 = 9, s2 = 9))
  sna_svdfF <- set_na_values(
    svdf,
    .values = list(s1 = 9, s3 = 2),
    .strict = FALSE
  )
  expect_equal(na_values(sna_svdfF), list(s1 = 9, s2 = NULL))
})

# set_na_range -----------------------------------------------------------------

test_that("set_na_range works correctly", {
  df <- dplyr::tibble(s1 = c("M", "M", "F", "F"), s2 = c(1, 1, 2, 9))
  svdf <- set_value_labels(df, s2 = c(yes = 1, no = 2))


  expect_error(set_na_range(svdf, s2 = 9))
  expect_error(set_na_range(svdf, n2 = c(9, Inf)))
  snr_svdf <- set_na_range(svdf, s2 = c(9, Inf))
  expect_equal(na_range(snr_svdf$s2), c(9, Inf))

  expect_error(set_na_range(svdf, s2 = c(9, Inf), s4 = c(0, 10)))
  snr_svdfF <-
    set_na_range(svdf, s2 = c(9, Inf), s4 = c(0, 10), .strict = FALSE)
  expect_equal(snr_svdfF, snr_svdf)

  expect_error(set_na_range(svdf, .values = c(s2 = 9)))
  expect_error(
    set_na_range(svdf, .values = list(s2 = c(9, Inf), s4 = c(0, 10)))
  )
  snrv_svdf <- set_na_range(svdf, .values = list(s2 = c(9, Inf)))
  expect_equal(snrv_svdf, snr_svdf)
  snrv_svdfF <- set_na_range(
    svdf,
    .values = list(s2 = c(9, Inf), s4 = c(0, 10)),
    .strict = FALSE
  )
  expect_equal(snrv_svdfF, snr_svdf)

  df <- dplyr::tibble(s1 = c(2, 4, 7, 9), s2 = c(1, 1, 2, 9))
  svdf <- set_value_labels(df, s2 = c(yes = 1, no = 2))
  sna_svdf <- set_na_range(svdf, .values = c(9L, 100L))
  expect_equal(na_range(sna_svdf), list(s1 = c(9, 100), s2 = c(9, 100)))

  x <- factor(1:5)
  expect_error(na_values(x) <- 1)
  expect_error(na_range(x) <- 4:5)

  v <- 1:10
  v <- set_na_range(v, 3, 5)
  v <- set_na_values(v, 8, 9)
  expect_equal(na_range(v), c(3, 5))
  expect_equal(na_values(v), c(8, 9))
})

test_that("about user NAs", {
  v <- labelled_spss(
    c(1, 2, 9, 3, 9, 1, NA),
    labels = c(yes = 1, no = 3, "don't know" = 9),
    na_values = 9
  )
  expect_equal(
    is.na(v),
    c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  expect_equal(
    is_user_na(v),
    c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    user_na_to_tagged_na(v) %>% format_tagged_na() %>% trimws(),
    c("1", "2", "NA(a)", "3", "NA(a)", "1", "NA")
  )
  expect_equal(
    user_na_to_na(letters),
    letters
  )

  x <- c(NA, 9, tagged_na("a"))
  na_values(x) <- 9
  expect_equal(is.na(x), c(TRUE, TRUE, TRUE))
  expect_equal(is_regular_na(x), c(TRUE, FALSE, FALSE))
  expect_equal(is_user_na(x), c(FALSE, TRUE, FALSE))
  expect_equal(is_tagged_na(x), c(FALSE, FALSE, TRUE))
})
