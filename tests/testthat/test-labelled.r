# var_label --------------------------------------------------------------

test_that("var_label works properly", {
  x <- 1:3
  var_label(x) <- "value"
  expect_equal(attr(x, "label"), "value")
  expect_equal(var_label(x), "value")
  var_label(x) <- NULL
  expect_null(attr(x, "label"))
  expect_null(var_label(x))
  expect_no_error(var_label(x) <- NA_character_)

  x <- 1:3
  x <- set_variable_labels(x, "value")
  expect_equal(attr(x, "label"), "value")
  x <- set_variable_labels(x, "something elese", .overwrite = FALSE)
  expect_equal(attr(x, "label"), "value")
  x <- set_variable_labels(x, .labels = "other value")
  expect_equal(attr(x, "label"), "other value")
  x <- set_variable_labels(x, .labels = "something else", .overwrite = FALSE)
  expect_equal(attr(x, "label"), "other value")
  x <- set_variable_labels(x, NULL)
  expect_null(attr(x, "label"))

  labs <- list(speed = NULL, dist = NULL)
  expect_no_error(
    set_variable_labels(cars, .labels = labs)
  )
  expect_no_error(
    var_label(cars) <- labs
  )

  labels <- list(dist = "BAR", speed = "FOO")
  data_labelled <- set_variable_labels(cars, .labels = labels)
  expect_identical(
    get_variable_labels(data_labelled),
    list(speed = "FOO", dist = "BAR")
  )

  labels <- list(dist = "BLUE", speed = "YELLOW")
  data_labelled <- data_labelled %>%
    set_variable_labels(.labels = labels, .overwrite = FALSE)
  expect_identical(
    get_variable_labels(data_labelled),
    list(speed = "FOO", dist = "BAR")
  )
})

test_that("var_label works on data.frame", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
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
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  expect_error(var_label(df) <- c("var1", "var2", "var3"))
  expect_error(var_label(df) <- list(x = "xx", z = "zz"))
  expect_error(
    df %>%
      set_variable_labels(.labels = list(x = "xx", z = "zz"))
  )
  expect_error(
    df %>%
      set_variable_labels(x = "ghj", z = "ggg")
  )
  # no error if .strict = FALSE
  expect_no_error(
    df %>%
      set_variable_labels(.labels = list(x = "xx", z = "zz"), .strict = FALSE)
  )
  expect_no_error(
    df %>%
      set_variable_labels(x = "ghj", z = "ggg", .strict = FALSE)
  )
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

test_that("labelled return an object of class haven_labelled", {
  x <- labelled(c(1, 2, 3), c(yes = 1, maybe = 2, no = 3))
  expect_true(is.labelled(x))
  expect_s3_class(x, "haven_labelled")
})

test_that("x must be numeric or character", {
  expect_error(labelled(TRUE))
})

test_that("x and labels must be compatible", {
  expect_error(labelled(1, "a"))
  expect_no_error(labelled(1, c(female = 2L, male = 1L)))
  expect_no_error(labelled(1L, c(female = 2, male = 1)))
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
  x <- labelled_spss(
    1:10,
    c(Good = 1, Bad = 8),
    na_values = c(9, 10),
    na_range = c(11, Inf)
  )
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

test_that("val_labels() null action", {
  x <- labelled(1:10, c(Good = 1, Bad = 8))

  val_labels(x, null_action = "labelled") <- NULL
  expect_true(inherits(x, "haven_labelled"))

  val_labels(x) <- NULL
  expect_false(inherits(x, "haven_labelled"))
})

test_that("value labels to NULL remove class if na_Values et na_range are NULL", { # nolint
  x <- labelled_spss(1:10, c(Good = 1, Bad = 8))
  val_labels(x) <- NULL
  expect_null(val_labels(x))
  expect_equal(match("labelled", names(attributes(x)), nomatch = 0), 0)
})

test_that("error with non character argument", {
  x <- 1
  expect_error(var_label(x) <- 1)
})

test_that("error with mutilple character argument", {
  x <- 1
  expect_error(var_label(x) <- c("a", "b"))
})

test_that("test if unlist argument works properly", {
  df <- data.frame(col1 = 1:2, col2 = 3:4, stringsAsFactors = FALSE)
  expect_equal(var_label(df, unlist = TRUE), c(col1 = "", col2 = ""))

  var_label(df) <- c("lb1", "lb2")
  expect_equal(var_label(df, unlist = TRUE), c(col1 = "lb1", col2 = "lb2"))
})


test_that("val_labels prefixed argument 100%", {
  v <- labelled(
    c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
    c(yes = 1, no = 3, "don't know" = 9)
  )
  vlv <- val_labels(v)
  vlvp <- val_labels(v, prefixed = TRUE)

  noms_vlvp <- names(vlvp)
  pos <- regexpr("] ", noms_vlvp)
  noms_vlvp <- substring(noms_vlvp, pos + 2)
  names(vlvp) <- noms_vlvp
  expect_equal(vlv, vlvp)
})

test_that("val_labels works for dataframe", {
  v <- labelled(
    c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
    c(yes = 1, no = 3, "don't know" = 9)
  )
  y <- 1:10
  df <- data.frame(v = v, y = y, stringsAsFactors = FALSE)
  res <- list(v = val_labels(v), y = NULL)
  expect_equal(val_labels(df), res)
})

test_that(" 'val_labels <-'  works for dataframe", {
  xhs <- labelled_spss(
    c(1:3, NA, 5:10),
    c(Good = 1, Bad = 8),
    na_values = c(9, 10),
    na_range = c(11, Inf)
  )
  num <- 1:10
  ch <- letters[1:10]
  fac <- factor(paste0("f", 1:10))
  df <- data.frame(
    xhs = xhs, num = num, ch = ch, fac = fac,
    stringsAsFactors = FALSE
  )

  expect_error(val_labels(df) <- c(one = 1))

  valeurs <- list(
    xhs = c(two = 2, five = 5),
    ch = c(leter_a = "a"),
    num = c(two = 2),
    fac = c(three = factor(2))
  )
  vldf <- df
  expect_error(val_labels(vldf) <- valeurs)

  valeurs <- list(
    xhs = c(two = 2, five = 5),
    ch = c(leter_a = "a"),
    num = c(two = 2)
  )
  vldf <- df
  expect_no_error(val_labels(vldf) <- valeurs)

  expect_null(val_labels(vldf)$fac)
  expect_equal(df$fac, vldf$fac)
  noms <- c("xhs", "num", "ch")
  expect_equal(val_labels(vldf)[noms], valeurs[noms])

  val_labels(df) <- NULL
  expect_true(all(sapply(val_labels(df), is.null)))
})

test_that("val_labels works with all NAs vectors", {
  x <- c(NA, NA)
  expect_message(
    val_labels(x) <- c(yes = 1, no = 2)
  )

  x <- c(NA, NA)
  expect_message(
    val_labels(x) <- c(yes = "y")
  )

  d <- data.frame(q = c(NA, NA))
  expect_message(
    val_labels(d) <- list(q = c(no = 1))
  )

  d <- data.frame(q = c(NA, NA))
  expect_message(
    d %>% set_value_labels(q = c(yes = "y"))
  )
})

test_that("val_label works for haven_labelled", {
  v <- labelled(
    c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
    c(yes = 1, no = 3, "don't know" = 9)
  )
  expect_equal(val_label(v, 2), NULL)
  expect_equal(val_label(v, 1), "yes")
  expect_equal(val_label(v, 1, prefixed = TRUE), "[1] yes")
  expect_error(val_label(v, 1:2))
})

test_that("val_label works for default", {
  num <- 1:3
  ch <- letters[1:3]
  expect_equal(val_label(num, 2), NULL)
  expect_error(val_lable(num, 1:2))
  expect_equal(val_label(ch, 1, prefixed = TRUE), NULL)
  expect_error(val_label(ch, 1:2))
})

test_that("val_label works for for dataframe", {
  xhs <- labelled_spss(
    c(1:3, NA, 5:10),
    c(Good = 1, Bad = 8),
    na_values = c(9, 10),
    na_range = c(11, Inf)
  )
  xh <- labelled(
    c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
    c(yes = 1, no = 3, "don't know" = 9)
  )
  num <- 1:10
  df <- data.frame(xhs = xhs, num = num, xh = xh, stringsAsFactors = FALSE)


  expect_true(all(sapply(val_label(df, 2), is.null)))
  expect_equal(
    val_label(df, 1),
    list(xhs = "Good", num = NULL, xh = "yes")
  )
  expect_equal(
    val_label(df, 3, prefixed = TRUE),
    list(xhs = NULL, num = NULL, xh = "[3] no")
  )
  expect_error(val_lable(df, 1:2))
})

test_that(" 'val_label<-' works properly", {
  xhs <- labelled_spss(
    c(1:3, NA, 5:10),
    c(Good = 1, Bad = 8),
    na_values = c(9, 10),
    na_range = c(11, Inf)
  )
  xh <- labelled(
    c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
    c(yes = 1, no = 3, "don't know" = 9)
  )
  num <- 1:10
  ch <- letters[1:10]

  expect_error(val_label(num, "a") <- "a")

  expect_error(val_label(xh, 12) <- c("one", "two"))
  expect_error(val_label(xhs, c(12, 13)) <- "twenty_two")

  df <- data.frame(
    xhs = xhs,
    num = num,
    xh = xh,
    ch = ch,
    stringsAsFactors = FALSE
  )
  expect_error(val_label(df, 2) <- 2)
  expect_error(val_label(df, 2) <- two)
  expect_error(val_label(df, 2) <- c("a", "b"))
  expect_error(val_label(df, 2:3) <- "a")

  sub_df <- df[, -match("ch", names(df))]

  v <- as.Date("2023-01-01")
  l <- as.Date(c("The first day of 2023" = "2023-01-01"))
  expect_error(val_labels(v) <- l)
})

test_that(" 'val_label<-.data.frame' works properly", {
  xhs <- labelled_spss(
    c(1:3, NA, 5:10),
    c(Good = 1, Bad = 8),
    na_values = c(9, 10),
    na_range = c(11, Inf)
  )
  num <- 1:10
  ch <- letters[1:10]
  df <- data.frame(xhs = xhs, num = num, ch = ch, stringsAsFactors = FALSE)

  valeurs <- list(xhs = "2", ch = "letter_a", num = "two")


  df_c <- df
  expect_error(val_label(df_c, 2) <- valeurs)
  expect_error(val_label(df_c, "a") <- valeurs)

  val_label(df_c, 2) <- valeurs[-2]
  val_label(df_c, "a") <- valeurs[2]
  res_labels <- list(
    xhs = c(Good = 1, Bad = 8, "2" = 2),
    num = c(two = 2),
    ch = c(letter_a = "a")
  )
  expect_equal(val_labels(df_c), res_labels)
})

# remove_labels --------------------------------------------------------------

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


test_that("remove_labels strips labelled attributes", {
  var <- labelled(c(1L, 98L, 99L), c(not_answered = 98L, not_applicable = 99L))
  exp <- c(1L, 98L, 99L)
  expect_equal(remove_labels(var), exp)
})

test_that("remove_labels returns variables not of class('labelled') unmodified", { # nolint
  var <- c(1L, 98L, 99L)
  expect_equal(remove_labels(var), var)
})

test_that("remove_labels works with data.frame", {
  var <- labelled(c(1L, 98L, 99L), c(not_answered = 98L, not_applicable = 99L))
  exp <- c(1L, 98L, 99L)
  df <- data.frame(var = var, exp = exp, stringsAsFactors = FALSE)
  rmdf <- remove_labels(df)
  expect_equal(rmdf$exp, exp)
  expect_equal(rmdf$var, exp)
})


test_that("remove_labels works with labelled_spss", {
  xhs <- haven::labelled_spss(
    c(1, 2, 3, NA, 99),
    c(t1 = 1, t2 = 2, Missing = 99),
    na_values = 99,
    na_range = c(99, Inf),
    label = "A test variable"
  )
  expect_null(var_label(remove_labels(xhs)))
  expect_false(identical(var_label(remove_labels(xhs)), var_label(xhs)))
  expect_null(val_labels(remove_labels(xhs)))
})

# remove_val_labels ------------------------------------------------------------


test_that("remove_labels works properly", {
  var <- labelled(
    c(1L, 98L, 99L),
    c(not_answered = 98L, not_applicable = 99L),
    label = "A variable label"
  )
  exp <- c(1L, 98L, 99L)
  df <- data.frame(var = var, exp = exp, stringsAsFactors = FALSE)
  rmdf <- remove_val_labels(df)
  expect_null(val_labels(rmdf$var))
  expect_false(identical(rmdf$var, exp))
  expect_equal(rmdf$exp, exp)
})

# remove_var_label ------------------------------------------------------------


test_that("remove_labels works properly", {
  var <- labelled(
    c(1L, 98L, 99L),
    c(not_answered = 98L, not_applicable = 99L),
    label = "A variable label"
  )
  exp <- c(1L, 98L, 99L)
  df <- data.frame(var = var, exp = exp, stringsAsFactors = FALSE)
  rmdf <- remove_var_label(df)
  expect_null(var_label(rmdf$var))
  expect_false(identical(rmdf$var, exp))
  expect_equal(val_labels(rmdf$var), val_labels(var))
  expect_equal(rmdf$exp, exp)
})


# sort_val_labels ---------------------------------------------------------

test_that("sort_val_labels works properly", {
  df <- data.frame(
    lab = labelled(c(1, 2, 3), c(maybe = 2, yes = 1, no = 3)), num = c(3, 1, 2),
    stringsAsFactors = FALSE
  )
  sdf <- sort_val_labels(df)
  expect_equal(
    val_labels(sdf),
    list(lab = c(yes = 1, maybe = 2, no = 3), num = NULL)
  )
  sdf <- sort_val_labels(df, decreasing = TRUE)
  expect_equal(
    val_labels(sdf),
    list(lab = c(no = 3, maybe = 2, yes = 1), num = NULL)
  )
  sdf <- sort_val_labels(df, "l")
  expect_equal(
    val_labels(sdf),
    list(lab = c(maybe = 2, no = 3, yes = 1), num = NULL)
  )
  sdf <- sort_val_labels(df, "l", TRUE)
  expect_equal(
    val_labels(sdf),
    list(lab = c(yes = 1, no = 3, maybe = 2), num = NULL)
  )

  x <- c(2, tagged_na("z"), 1, tagged_na("a"))
  val_labels(x) <-
    c(no = 2, refused = tagged_na("z"), yes = 1, dk = tagged_na("a"))
  expect_equal(
    sort_val_labels(x, according_to = "v") %>%
      val_labels() %>%
      format_tagged_na() %>%
      trimws(),
    c("1", "2", "NA(a)", "NA(z)"),
    ignore_attr = "names"
  )
  expect_equal(
    sort_val_labels(x, according_to = "l") %>% val_labels() %>% names(),
    c("dk", "no", "refused", "yes")
  )
})

# remove_user_na --------------------------------------------------------------


test_that("remove_user_na works properly", {
  var <- labelled(
    c(1L, 2L, NA, 98L, 99L),
    c(not_answered = 98L, not_applicable = 99L),
    label = "A variable label"
  )
  exp <- c(1L, 2L, NA, 98L, 99L)
  xhs <- haven::labelled_spss(
    c(1, 2, NA, 98, 99),
    c(t1 = 1, t2 = 2, Missing = 99),
    na_values = 99,
    na_range = c(99, Inf),
    label = "A test variable"
  )
  df <- data.frame(var = var, exp = exp, xhs = xhs, stringsAsFactors = FALSE)
  rmtdf <- remove_user_na(df, user_na_to_na = TRUE)
  expect_equal(rmtdf$var, var)
  expect_equal(rmtdf$exp, exp)
  expect_null(na_values(rmtdf$xhs))
  expect_equal(rmtdf$exp, exp)

  rmfdf <- remove_user_na(df, user_na_to_na = FALSE)
  expect_false(is.null(var_label(rmfdf$var)))

  rmfdf <- remove_user_na(df, user_na_to_tagged_na = TRUE)
  expect_equal(
    na_tag(rmfdf$xhs),
    c(NA, NA, NA, NA, "a")
  )

  x <- labelled_spss(1:100, na_range = c(50, 100))
  expect_warning(remove_user_na(x, user_na_to_tagged_na = TRUE))
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

  df3 <- to_factor(df, labelled_only = FALSE)
  expect_true(is.factor(df3$y))
  expect_true(is.factor(df3$z))
})

test_that("to_factor does not change a factor", {
  x <- factor(1:2)
  expect_equal(to_factor(x), x)
})

test_that("to_factor keeps labels", {
  x <- 1:2
  lab_name <- "vector"
  var_label(x) <- lab_name
  expect_equal(var_label(to_factor(x)), lab_name)
})

test_that("to_factor boolean parameters", {
  x1 <- haven::labelled_spss(
    c(1, 2, 3, 5, 4, NA, 99),
    c(t1 = 1, t2 = 2, t5 = 5, Missing = 99),
    na_values = 99
  )

  tfx <- to_factor(x1, user_na_to_na = TRUE)
  expect_equal(which(is.na(tfx)), 6:7)
  expect_equal(levels(tfx), c("t1", "t2", "3", "4", "t5"))


  tfx <- to_factor(x1, nolabel_to_na = TRUE)
  expect_equal(which(is.na(tfx)), c(3, 5, 6))
  expect_equal(levels(tfx), c("t1", "t2", "t5", "Missing"))

  tfx <- to_factor(x1[1:3], drop_unused_labels = FALSE)
  expect_equal(levels(tfx), c("t1", "t2", "3", "t5", "Missing"))

  tfx <- to_factor(x1[1:3], drop_unused_labels = TRUE)
  expect_equal(levels(tfx), c("t1", "t2", "3"))
})

test_that("to_factor parameters : sort_levels + levels", {
  x1 <- haven::labelled_spss(
    c(1, 2, 3, 5, 4, NA, 99),
    c(t1 = 1, t2 = 2, t5 = 5, Missing = 99),
    na_values = 99
  )

  tfx <- to_factor(x1, sort_levels = "auto")
  expect_equal(levels(tfx), c("t1", "t2", "3", "4", "t5", "Missing"))

  tfx <- to_factor(x1, sort_levels = "none")
  expect_equal(levels(tfx), c("t1", "t2", "t5", "Missing", "3", "4"))

  tfx <- to_factor(x1, sort_levels = "labels")
  expect_equal(levels(tfx), c("3", "4", "Missing", "t1", "t2", "t5"))

  tfx <- to_factor(x1, sort_levels = "values")
  expect_equal(levels(tfx), c("t1", "t2", "3", "4", "t5", "Missing"))

  tfx <- to_factor(x1, levels = "labels")
  expect_equal(levels(tfx), c("t1", "t2", "3", "4", "t5", "Missing"))

  tfx <- to_factor(x1, levels = "values")
  expect_equal(levels(tfx), c("1", "2", "3", "4", "5", "99"))

  tfx <- to_factor(x1, levels = "prefixed")
  expect_equal(
    levels(tfx),
    c("[1] t1", "[2] t2", "[3] 3", "[4] 4", "[5] t5", "[99] Missing")
  )
})

test_that("to_factor() and tagged NAs", {
  x <- c(1, 2, tagged_na("a"), 1, tagged_na("z"), 2, tagged_na("a"), NA)
  val_labels(x) <- c(
    yes = 1, no = 2,
    missing = tagged_na("a"),
    toto = NA
  )

  expect_equal(
    to_factor(x),
    structure(c(1L, 2L, NA, 1L, NA, 2L, NA, NA),
      .Label = c("yes", "no"), class = "factor"
    )
  )
  expect_equal(
    to_factor(x, explicit_tagged_na = TRUE),
    structure(c(1L, 2L, 4L, 1L, 5L, 2L, 4L, 3L),
      .Label = c("yes", "no", "toto", "missing", "NA(z)"),
      class = "factor"
    )
  )
})

# to_character -----------------------------------------------------------------

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

test_that("to_character produce an appropriate character vector", {
  x <- labelled(c(1, 1, 2), c(yes = 1, no = 2))
  expect_equal(class(to_character(x)), "character")
  expect_equal(to_character(x), c("yes", "yes", "no"))
})

test_that("to_character default (100%)", {
  x <- 1:3
  expect_equal(class(to_character(x)), "character")
  expect_equal(to_character(x), as.character(x))
})

test_that("to_character.double and explicit_tagged_na", {
  x <- c(1:3, tagged_na("a"), tagged_na("z"))
  expect_equal(
    to_character(x),
    c("1", "2", "3", NA, NA)
  )
  expect_equal(
    to_character(x, explicit_tagged_na = TRUE),
    c("1", "2", "3", "NA(a)", "NA(z)")
  )
})

# set_value_labels and add_value_labels ---------------------------------------

test_that("set_value_labels replaces all value labels", {
  df <- data.frame(
    s1 = c("M", "M", "F"),
    s2 = c(1, 1, 2),
    x = 1:3,
    stringsAsFactors = FALSE
  )
  df <- set_value_labels(
    df,
    s1 = c(Male = "M", Female = "F"),
    s2 = c(Yes = 1, No = 2)
  )
  expect_equal(val_labels(df$s1), c(Male = "M", Female = "F"))
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2))
  df <- df %>%
    set_value_labels(s2 = c(test = 1), x = c(test = 1), .overwrite = FALSE)
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2))
  expect_equal(val_labels(df$x), c(test = 1))
  df <- set_value_labels(df, s2 = c(Yes = 1, Unknown = 9))
  expect_equal(val_labels(df$s2), c(Yes = 1, Unknown = 9))
  df <- set_value_labels(df, s1 = NULL)
  df <- set_value_labels(df, s2 = NULL, .null_action = "lab")
  expect_false(inherits(df$s1, "haven_labelled"))
  expect_true(inherits(df$s2, "haven_labelled"))

  v <- set_value_labels(1:10, c(low = 1, high = 10))
  expect_equal(val_labels(v), c(low = 1, high = 10))

  v <- set_value_labels(1:10, low = 1, high = 10)
  expect_equal(val_labels(v), c(low = 1, high = 10))
  v <- set_value_labels(v, test = 1, .overwrite = FALSE)
  expect_equal(val_labels(v), c(low = 1, high = 10))

  v <- set_value_labels(1:10, .labels = c(low = 1, high = 10))
  expect_equal(val_labels(v), c(low = 1, high = 10))
  v <- set_value_labels(v, .labels = c(test = 1), .overwrite = FALSE)
  expect_equal(val_labels(v), c(low = 1, high = 10))

  v <- set_value_labels(v, NULL)
  expect_null(val_labels(v))
})

test_that("set_value_labels errors", {
  df <- data.frame(
    s1 = c("M", "M", "F"),
    s2 = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  expect_error(
    df %>%
      set_value_labels(
        s1 = c(Male = "M", Female = "F"),
        s3 = c(Yes = 1, No = 2)
      )
  )
  expect_error(
    df %>%
      set_value_labels(
        .labels = list(
          s1 = c(Male = "M", Female = "F"),
          s3 = c(Yes = 1, No = 2)
        )
      )
  )
  # no error if .strict = FALSE
  expect_no_error(
    df %>%
      set_value_labels(
        s1 = c(Male = "M", Female = "F"),
        s3 = c(Yes = 1, No = 2),
        .strict = FALSE
      )
  )
  expect_no_error(
    df %>%
      set_value_labels(
        .labels = list(
          s1 = c(Male = "M", Female = "F"),
          s3 = c(Yes = 1, No = 2)
        ),
        .strict = FALSE
      )
  )
})

test_that("add_value_labels errors", {
  df <- data.frame(
    s1 = c("M", "M", "F"),
    s2 = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  expect_error(
    df %>%
      add_value_labels(
        s1 = c(Male = "M", Female = "F"),
        s3 = c(Yes = 1, No = 2)
      )
  )
  expect_error(
    df %>%
      add_value_labels(
        .labels = list(
          s1 = c(Male = "M", Female = "F"),
          s3 = c(Yes = 1, No = 2)
        )
      )
  )
  # no error if .strict = FALSE
  expect_no_error(
    df %>%
      add_value_labels(
        s1 = c(Male = "M", Female = "F"),
        s3 = c(Yes = 1, No = 2),
        .strict = FALSE
      )
  )
  expect_no_error(
    df %>%
      add_value_labels(
        .labels = list(
          s1 = c(Male = "M", Female = "F"),
          s3 = c(Yes = 1, No = 2)
        ),
        .strict = FALSE
      )
  )

  expect_error(add_value_labels(df, s1 = c("F", Male = "M")))
})

test_that("add_value_labels and remove_value_labels updates the list of value labels", { # nolint
  df <- data.frame(
    s1 = c("M", "M", "F"),
    s2 = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  df <- set_value_labels(
    df,
    s1 = c(Male = "M", Female = "F"),
    s2 = c(Yesss = 1, No = 2)
  )
  df <- add_value_labels(df, s2 = c(Yes = 1, Unknown = 9))
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2, Unknown = 9))
  df <- remove_value_labels(df, s2 = 9)
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2))

  expect_error(remove_value_labels(df, 9))

  v <- set_value_labels(1:10, low = 1, high = 10)
  v <- add_value_labels(v, middle = 5)
  v <- remove_value_labels(v, 10)
  expect_equal(val_labels(v), c(low = 1, middle = 5))
})

# set_variable_labels  --------------------------------------------------------

test_that("set_variable_labels updates variable labels", {
  df <- data.frame(
    s1 = c("M", "M", "F"),
    s2 = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  df <- set_variable_labels(df, s1 = "Sex", s2 = "Question")
  expect_equal(var_label(df$s1), "Sex")
  df <- set_variable_labels(df, s2 = NULL)
  expect_null(var_label(df$s2))
})

# missing values --------------------------------------------------------------

test_that("it is possible to define missing values if no value labels were defined", { # nolint
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

# recode (dplyr) ---------------------------------------------------------------
test_that("dplyr::recode could be applied to numeric labelled vector", {
  x <- dplyr::recode(labelled(1:3, c(yes = 1, no = 2)), `3` = 2L)
  expect_equal(x, labelled(c(1L, 2L, 2L), c(yes = 1, no = 2)))
})

test_that("dplyr::recode could be applied to character labelled vector", {
  x <- dplyr::recode(
    labelled(c("a", "b", "c"), c(yes = "a", no = "b")),
    c = "b"
  )
  expect_equal(x, labelled(c("a", "b", "b"), c(yes = "a", no = "b")))
})

test_that("dplyr::recode could handle NA with .combine_value_labels", {
  x <- labelled(c(NA, 1:3), c(yes = 1, maybe = 2, no = 3))
  y <- x %>% dplyr::recode(`2` = 0L, .combine_value_labels = TRUE)
  expect_true(all(c(0, 1, 3) %in% val_labels(y)))

  y <- x %>% dplyr::recode(`2` = 0L, `3` = 0L, .combine_value_labels = TRUE)
  expect_true(all(c(0, 1) %in% val_labels(y)))
  expect_equal(val_label(y, 0), "maybe / no")
})


# update_labelled ----------------------------------------

test_that("update_labelled update previous haven's labelled objects but not Hmisc's labelled objects", { # nolint
  vhaven <- structure(
    1:4,
    label = "label",
    labels = c(No = 1, Yes = 2),
    class = "labelled"
  )
  vHmisc <- structure(1:4, label = "label", class = "labelled")

  expect_s3_class(update_labelled(vhaven), "haven_labelled")
  expect_s3_class(update_labelled(vHmisc), "labelled")

  df <- dplyr::tibble(vhaven, vHmisc)
  expect_s3_class(update_labelled(df)$vhaven, "haven_labelled")
  expect_s3_class(update_labelled(df)$vHmisc, "labelled")
})

test_that("update_labelled update to haven_labelled_spss if there are na values", { # nolint
  v1 <- structure(1:4,
    label = "label", labels = c(No = 1, Yes = 2),
    na_values = c(8, 9), class = c("labelled_spss", "labelled")
  )
  v2 <- structure(1:4,
    label = "label", labels = c(No = 1, Yes = 2),
    na_range = c(8, 9), class = c("labelled_spss", "labelled")
  )

  expect_s3_class(update_labelled(v1), "haven_labelled_spss")
  expect_s3_class(update_labelled(v1), "haven_labelled_spss")
})

test_that("update_labelled preserve variable and value labels", {
  v <- structure(
    1:4,
    label = "variable label",
    labels = c(No = 1, Yes = 2),
    class = "labelled"
  )

  expect_equal(var_label(update_labelled(v)), "variable label")
  expect_equal(val_labels(update_labelled(v)), c(No = 1, Yes = 2))
})

test_that("update_labelled do nothing if it's not a labelled vector", {
  x <- 1:10
  expect_equal(update_labelled(x), x)
})

test_that("update_labelled works with labelled from haven 2.0", {
  data(x_haven_2.0)
  x <- labelled(c(1, 2, 1, 2, 10, 9), c(Unknown = 9, Refused = 10))
  expect_false(identical(x, x_haven_2.0))
  up_x_haven_2.0 <- update_labelled(x_haven_2.0)
  expect_equal(x, up_x_haven_2.0)

  data(x_spss_haven_2.0)
  x2 <- labelled_spss(
    1:10, c(Good = 1, Bad = 8),
    na_range = c(9, Inf), label = "Quality rating"
  )
  expect_false(identical(x2, x_spss_haven_2.0))
  up_x_spss_haven_2.0 <- update_labelled(x_spss_haven_2.0)
  expect_equal(x2, up_x_spss_haven_2.0)
})



# remove_attributes ------------------------------------------------------------

test_that("remove_attributes does not transform characters into factors", {
  d <- data.frame(
    ch = structure(letters[1:2], some_attribute = TRUE),
    stringsAsFactors = FALSE
  )
  d <- remove_attributes(d, "some_attribute")
  expect_true(is.character(d$ch))
})



# unlabelled ------------------------------------------------------------------

test_that("unlabelled works correctly", {
  df <- data.frame(
    a = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2)),
    b = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2, DK = 3)),
    c = labelled(c("a", "a", "b", "c"), labels = c(No = "a", Yes = "b")),
    stringsAsFactors = FALSE
  )

  df <- unlabelled(df)
  expect_equal(class(df$a), "numeric")
  expect_s3_class(df$b, "factor")
  expect_equal(class(df$c), "character")

  v <- labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2, DK = 3))
  expect_s3_class(unlabelled(v), "factor")

  v <- labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2))
  expect_false(inherits(unlabelled(v), "haven_labelled"))

  expect_false(is.factor(unlabelled(1:4)))
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

  r <- dplyr::recode(
    x,
    `1` = 1L, `2` = 1L, `3` = 2L, `4` = 2L,
    .combine_value_labels = TRUE
  )
  expect_equal(val_labels(r), c("a / b" = 1L, "c / d" = 2L))

  r <- dplyr::recode(x, `2` = 1L, `4` = 3L, .combine_value_labels = TRUE)
  expect_equal(val_labels(r), c("a / b" = 1L, "c / d" = 3L))

  r <- dplyr::recode(
    x,
    `2` = 1L, `4` = 3L,
    .combine_value_labels = TRUE,
    .sep = " or "
  )
  expect_equal(val_labels(r), c("a or b" = 1L, "c or d" = 3L))

  y <- labelled(1:4, c(a = 1))
  r <- dplyr::recode(y, `2` = 1L, `4` = 3L, .combine_value_labels = TRUE)
  expect_equal(val_labels(r), c(a = 1L))
})

# tidy dots --------------------------------------------------------------

test_that("functions with dots accept tidy evaluation (`!!!` operator)", {
  df <- data.frame(
    s1 = c("M", "M", "F"),
    s2 = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  variable_list <- list(s1 = "Sex", s2 = "Question")
  df <- set_variable_labels(df, !!!variable_list)
  expect_equal(var_label(df$s1), "Sex")
  expect_equal(var_label(df$s2), "Question")

  df <- data.frame(
    s1 = c("M", "M", "F"),
    s2 = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  labels_list <- list(
    s1 = c(Male = "M", Female = "F"),
    s2 = c(Yes = 1, No = 2)
  )
  df <- set_value_labels(df, !!!labels_list)
  expect_equal(val_labels(df$s1), c(Male = "M", Female = "F"))
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2))

  df <- data.frame(
    s1 = c("M", "M", "F"),
    s2 = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  df <- set_value_labels(
    df,
    s1 = c(Male = "M", Female = "F"),
    s2 = c(Yesss = 1, No = 2)
  )
  added_values_list <- list(s2 = c(Yes = 1, Unknown = 9))
  df <- add_value_labels(df, !!!added_values_list)
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2, Unknown = 9))
  removed_values_list <- list(s2 = 9)
  df <- remove_value_labels(df, !!!removed_values_list)
  expect_equal(val_labels(df$s2), c(Yes = 1, No = 2))
})


# drop_unused_value_labels ------------------------------------------------

test_that("drop_unused_value_labels works properly with data.frame", {
  x <- labelled(c(1, 2, 2, 1), c(yes = 1, no = 2, maybe = 3))
  y <- 1:4
  df <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
  ddf <- drop_unused_value_labels(df)
  expect_false(identical(ddf$x, x))
  expect_equal(ddf$y, y)
  expect_false(identical(val_labels(ddf$x), val_labels(x)))
  expect_equal(val_labels(ddf$x), val_labels(x)[-3])
})


# nolabel_to_na -----------------------------------------------------------

test_that("nolabel_to_na works properly", {
  x <- labelled(c(1, 2, 9, 1, 9), c(yes = 1, no = 2))
  y <- 1:5
  df <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
  nldf <- nolabel_to_na(df)
  expect_false(identical(nldf$x, x))
  expect_equal(nldf$y, y)
  expect_equal(which(is.na(nldf$x)), c(3L, 5L))
})

# val_labels_to_na -----------------------------------------------------------

test_that("val_labels_to_na works properly", {
  x <- labelled(c(1, 2, 9, 1, 9), c(dk = 9))
  y <- 1:5
  df <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
  vldf <- val_labels_to_na(df)
  expect_false(identical(vldf$x, x))
  expect_equal(vldf$y, y)
  expect_null(val_labels(vldf$x))
  expect_equal(which(is.na(vldf$x)), c(3L, 5L))
})


# names_prefixed_by_values ------------------------------------------------


test_that("names_prefixed_by_values works properly", {
  df <- dplyr::tibble(
    c1 = labelled(c("M", "M", "F"), c(Male = "M", Female = "F")),
    c2 = labelled(c(1, 1, 2), c(Yes = 1, No = 2)),
  )

  res_names_prefixed <- list(
    c1 = c("[M] Male", "[F] Female"),
    c2 = c("[1] Yes", "[2] No")
  )
  expect_equal(
    names_prefixed_by_values(val_labels(df)),
    res_names_prefixed,
    ignore_attr = "names"
  )

  expect_null(names_prefixed_by_values(NULL))
})

test_that("null_action in var_label() works as expected", {
  df <- datasets::iris %>%
    set_variable_labels(
      Petal.Length = "length of petal",
      Petal.Width = "width of petal"
    )
  expect_equal(
    var_label(df),
    list(
      Sepal.Length = NULL,
      Sepal.Width = NULL,
      Petal.Length = "length of petal",
      Petal.Width = "width of petal",
      Species = NULL
    )
  )
  expect_equal(
    var_label(df, null_action = "fi"),
    list(
      Sepal.Length = "Sepal.Length",
      Sepal.Width = "Sepal.Width",
      Petal.Length = "length of petal",
      Petal.Width = "width of petal",
      Species = "Species"
    )
  )
  expect_equal(
    var_label(df, null_action = "na"),
    list(
      Sepal.Length = NA_character_,
      Sepal.Width = NA_character_,
      Petal.Length = "length of petal",
      Petal.Width = "width of petal",
      Species = NA_character_
    )
  )
  expect_equal(
    var_label(df, null_action = "empty"),
    list(
      Sepal.Length = "",
      Sepal.Width = "",
      Petal.Length = "length of petal",
      Petal.Width = "width of petal",
      Species = ""
    )
  )
  expect_equal(
    var_label(df, null_action = "skip"),
    list(
      Petal.Length = "length of petal",
      Petal.Width = "width of petal"
    )
  )
  expect_error(var_label(df$Species, null_action = "skip"))
})

test_that("var_label works with packed columns", {
  d <- iris %>%
    tidyr::as_tibble() %>%
    tidyr::pack(
      Sepal = starts_with("Sepal"),
      Petal = starts_with("Petal"),
      .names_sep = "."
    )

  d <- d %>% set_variable_labels(Sepal = "Label of the Sepal df-column")
  expect_equal(
    label_attribute(d$Sepal),
    "Label of the Sepal df-column"
  )

  d$Petal <- d$Petal %>%
    set_variable_labels(
      Length = "Petal length",
      Width = "Petal width"
    )
  expect_equal(
    label_attribute(d$Petal$Length),
    "Petal length"
  )

  expect_equal(
    length(var_label(d)),
    3L
  )

  expect_equal(
    length(var_label(d, recurse = TRUE)),
    3L
  )

  expect_equal(
    length(var_label(d, recurse = TRUE, unlist = TRUE)),
    5L
  )
})
