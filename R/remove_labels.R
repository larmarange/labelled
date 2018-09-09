#' Remove variable label, value labels and user defined missing values
#'
#' Use \code{remove_var_label} to remove variable label, \code{remove_val_labels}
#' to remove value labels, \code{remove_user_na} to remove user defined missing values (na_values and na_range)
#' and \code{remove_labels} to remove all.
#'
#' @param x A vector or a data frame.
#' @param user_na_to_na Convert user defined missing values into \code{NA}?
#' @details
#' Be careful with \code{remove_user_na} and \code{remove_labels}, user defined missing values
#' will not be automatically converted to \code{NA}, except if you specify
#' \code{user_na_to_na = TRUE}.
#' \code{user_na_to_na(x)} is an equivalent of \code{remove_user_na(x, user_na_to_na = TRUE)}.
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
remove_labels <- function(x, user_na_to_na = FALSE) {
  UseMethod("remove_labels")
}


#' @export
remove_labels.default <- function(x, user_na_to_na = FALSE) {
  var_label(x) <- NULL
  val_labels(x) <- NULL
  attr(x,"format.spss") <- NULL
  x
}

#' @export
remove_labels.haven_labelled_spss <- function(x, user_na_to_na = FALSE) {
  x <- remove_user_na(x, user_na_to_na = user_na_to_na)
  var_label(x) <- NULL
  val_labels(x) <- NULL
  attr(x,"format.spss") <- NULL
  x
}


#' @export
remove_labels.data.frame <- function(x, user_na_to_na = FALSE) {
  x[] <- lapply(x, remove_labels, user_na_to_na = user_na_to_na)
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
