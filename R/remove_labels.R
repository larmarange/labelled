#' Remove variable label, value labels and user defined missing values
#'
#' Use `remove_var_label()` to remove variable label, `remove_val_labels()`
#' to remove value labels, `remove_user_na()` to remove user defined missing values (*na_values* and *na_range*)
#' and `remove_labels()` to remove all.
#'
#' @param x A vector or a data frame.
#' @param user_na_to_na Convert user defined missing values into `NA`?
#' @param keep_var_label Keep variable label?
#' @details
#' Be careful with `remove_user_na()` and `remove_labels()`, user defined missing values
#' will not be automatically converted to `NA`, except if you specify
#' `user_na_to_na = TRUE`.
#' `user_na_to_na(x)` is an equivalent of `remove_user_na(x, user_na_to_na = TRUE)`.
#'
#' If you prefer to convert variables with value labels into factors, use [to_factor()]
#' or use [unlabelled()].
#' @examples
#' x1 <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
#' var_label(x1) <- "A variable"
#' x1
#'
#' x2 <- remove_labels(x1)
#' x2
#' x3 <- remove_labels(x1, user_na_to_na = TRUE)
#' x3
#' x4 <- remove_user_na(x1, user_na_to_na = TRUE)
#' x4
#' @export
remove_labels <- function(x, user_na_to_na = FALSE, keep_var_label = FALSE) {
  UseMethod("remove_labels")
}


#' @export
remove_labels.default <- function(x, user_na_to_na = FALSE, keep_var_label = FALSE) {
  if (!keep_var_label) var_label(x) <- NULL
  val_labels(x) <- NULL
  attr(x,"format.spss") <- NULL
  x
}

#' @export
remove_labels.haven_labelled_spss <- function(x, user_na_to_na = FALSE, keep_var_label = FALSE) {
  x <- remove_user_na(x, user_na_to_na = user_na_to_na)
  if (!keep_var_label) var_label(x) <- NULL
  val_labels(x) <- NULL
  attr(x,"format.spss") <- NULL
  x
}


#' @export
remove_labels.data.frame <- function(x, user_na_to_na = FALSE, keep_var_label = FALSE) {
  x[] <- lapply(
    x, remove_labels,
    user_na_to_na = user_na_to_na,
    keep_var_label = keep_var_label
  )
  x
}

#' @rdname remove_labels
#' @export
remove_var_label <- function(x) {
  UseMethod("remove_var_label")
}


#' @export
remove_var_label.default <- function(x) {
  var_label(x) <- NULL
  x
}

#' @export
remove_var_label.data.frame <- function(x) {
  x[] <- lapply(x, remove_var_label)
  x
}

#' @rdname remove_labels
#' @export
remove_val_labels <- function(x) {
  UseMethod("remove_val_labels")
}


#' @export
remove_val_labels.default <- function(x) {
  val_labels(x) <- NULL
  x
}

#' @export
remove_val_labels.data.frame <- function(x) {
  x[] <- lapply(x, remove_val_labels)
  x
}

#' @rdname remove_labels
#' @export
remove_user_na <- function(x, user_na_to_na = FALSE) {
  UseMethod("remove_user_na")
}


#' @export
remove_user_na.default <- function(x, user_na_to_na = FALSE) {
  # do nothing
  x
}

#' @export
remove_user_na.haven_labelled_spss <- function(x, user_na_to_na = FALSE) {
  if (user_na_to_na)
    x[is.na(x)] <- NA
  else if (!is.null(na_values(x)) | !is.null(na_range(x)))
    message("Some user defined missing values have been removed but not converted to NA.")
  na_values(x) <- NULL
  na_range(x) <- NULL
  x
}

#' @export
remove_user_na.data.frame <- function(x, user_na_to_na = FALSE) {
  x[] <- lapply(x, remove_user_na, user_na_to_na = user_na_to_na)
  x
}
