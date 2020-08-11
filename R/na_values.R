#' Get / Set SPSS missing values
#'
#' @param x A vector.
#' @param value A vector of values that should also be considered as missing
#' (for `na_values`) or a numeric vector of length two giving the (inclusive)
#' extents of the range (for `na_values`, use `-Inf` and `Inf` if you
#' want the range to be open ended).
#' @details
#' See [haven::labelled_spss()] for a presentation of SPSS's user defined missing values.
#' Note that [base::is.na()] will return `TRUE` for user defined misssing values.
#' You can use [user_na_to_na()] to convert user defined missing values to `NA`.
#' @return
#'   `na_values()` will return a vector of values that should also be considered as missing.
#'   `na_range()` will return a numeric vector of length two giving the (inclusive)
#'     extents of the range.
#' @seealso [haven::labelled_spss()], [user_na_to_na()]
#' @examples
#' v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9))
#' v
#' na_values(v) <- 9
#' na_values(v)
#' v
#' is.na(v)
#' user_na_to_na(v)
#' na_values(v) <- NULL
#' v
#' na_range(v) <- c(5, Inf)
#' na_range(v)
#' v
#' user_na_to_na(v)
#' @export
na_values <- function(x) {
  UseMethod("na_values")
}

#' @export
na_values.default <- function(x) {
  # return nothing
  NULL
}

#' @export
na_values.haven_labelled_spss <- function(x) {
  attr(x, "na_values", exact = TRUE)
}

#' @export
na_values.data.frame <- function(x) {
  lapply(x, na_values)
}

#' @rdname na_values
#' @export
`na_values<-` <- function(x, value) {
  UseMethod("na_values<-")
}

#' @export
`na_values<-.default` <- function(x, value) {
  if (!is.null(value))
    x <- labelled_spss(x, val_labels(x), na_values = value, na_range = attr(x, "na_range"), label = var_label(x))
  # else do nothing
  x
}

#' @export
`na_values<-.haven_labelled` <- function(x, value) {
  if (is.null(value)) {
    attr(x, "na_values") <- NULL
    if (is.null(attr(x, "na_range"))) {
      x <- labelled(x, val_labels(x), label = var_label(x))
    }
  } else {
    x <- labelled_spss(x, val_labels(x), na_values = value, na_range = attr(x, "na_range"), label = var_label(x))
  }
  x
}

#' @rdname na_values
#' @export
na_range <- function(x) {
  UseMethod("na_range")
}

#' @export
na_range.default <- function(x) {
  # return nothing
  NULL
}

#' @export
na_range.haven_labelled_spss <- function(x) {
  attr(x, "na_range", exact = TRUE)
}

#' @export
na_range.data.frame <- function(x) {
  lapply(x, na_range)
}

#' @rdname na_values
#' @export
`na_range<-` <- function(x, value) {
  UseMethod("na_range<-")
}

#' @export
`na_range<-.default` <- function(x, value) {
  if (!is.null(value))
    x <- labelled_spss(x, val_labels(x), na_values = attr(x, "na_values"), na_range = value, label = var_label(x))
  # else do nothing
  x
}

#' @export
`na_range<-.haven_labelled` <- function(x, value) {
  if (is.null(value)) {
    attr(x, "na_range") <- NULL
    if (is.null(attr(x, "na_values"))) {
      x <- labelled(x, val_labels(x), label = var_label(x))
    }
  } else {
    x <- labelled_spss(x, val_labels(x), na_values = attr(x, "na_values"), na_range = value, label = var_label(x))
  }
  x
}


#' @rdname na_values
#' @param .data a data frame
#' @param ... name-value pairs of missing values (see examples)
#' @note
#'   `set_na_values()` and `set_na_range()` could be used with \pkg{dplyr} syntax.
#' @return
#'  `set_na_values()` and `set_na_range()` will return an updated
#'  copy of `.data`.
#' @examples
#' if (require(dplyr)) {
#'   # setting value labels
#'   df <- tibble(s1 = c("M", "M", "F", "F"), s2 = c(1, 1, 2, 9)) %>%
#'     set_value_labels(s2 = c(yes = 1, no = 2)) %>%
#'     set_na_values(s2 = 9)
#'   na_values(df)
#'
#'   # removing missing values
#'   df <- df %>% set_na_values(s2 = NULL)
#'   df$s2
#' }
#' @export
set_na_values <- function(.data, ...) {
  values <- list(...)
  if (!all(names(values) %in% names(.data)))
    stop("some variables not found in .data")

  for (v in names(values))
    na_values(.data[[v]]) <- values[[v]]

  .data
}

#' @rdname na_values
#' @export
set_na_range <- function(.data, ...) {
  values <- list(...)
  if (!all(names(values) %in% names(.data)))
    stop("some variables not found in .data")

  for (v in names(values))
    na_range(.data[[v]]) <- values[[v]]

  .data
}



#' @rdname na_values
#' @export
user_na_to_na <- function(x) {
  UseMethod("user_na_to_na")
}

#' @export
user_na_to_na.default <- function(x) {
  # do nothing
  x
}

#' @export
user_na_to_na.haven_labelled_spss <- function(x) {
  remove_user_na(x, user_na_to_na = TRUE)
}

#' @export
user_na_to_na.data.frame <- function(x) {
  x[] <- lapply(x, user_na_to_na)
  x
}


