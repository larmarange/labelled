#' Remove variable and value labels
#'
#' This function removes all variable and values labels.
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
#' @details
#' \code{zap_labels} is equivalent to \code{remove_labels}.
#' @export
zap_labels <- function(x) {
  UseMethod("remove_labels")
}
