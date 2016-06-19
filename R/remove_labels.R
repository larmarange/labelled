#' Remove variable and value labels
#'
#' Use \code{remove_var_label} to remove variable label, \code{remove_val_labels}
#' to remove value labels and \code{remove_labels} to remove both variable and
#' value labels.
#'
#' @param x A vector or a data frame.
#' @export
remove_labels <- function(x) {
  UseMethod("remove_labels")
}


#' @rdname remove_labels
#' @export
remove_labels.default <- function(x) {
  var_label(x) <- NULL
  val_labels(x) <- NULL
  x
}

#' @rdname remove_labels
#' @export
remove_labels.data.frame <- function(x) {
  x[] <- lapply(x, remove_labels)
  x
}

#' @rdname remove_labels
#' @export
remove_var_label <- function(x) {
  UseMethod("remove_var_label")
}


#' @rdname remove_labels
#' @export
remove_var_label.default <- function(x) {
  var_label(x) <- NULL
  x
}

#' @rdname remove_labels
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


#' @rdname remove_labels
#' @export
remove_val_labels.default <- function(x) {
  val_labels(x) <- NULL
  x
}

#' @rdname remove_labels
#' @export
remove_val_labels.data.frame <- function(x) {
  x[] <- lapply(x, remove_val_labels)
  x
}
