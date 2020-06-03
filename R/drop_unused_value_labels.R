#' Drop unused value labels
#'
#' Drop value labels associated to a value not present in the data.
#'
#' @param x A vector or a data frame.
#' @examples
#' x <- labelled(c(1, 2, 2, 1), c(yes = 1, no = 2, maybe = 3))
#' x
#' drop_unused_value_labels(x)
#' @export
drop_unused_value_labels <- function(x) {
  UseMethod("drop_unused_value_labels")
}


#' @export
drop_unused_value_labels.default <- function(x) {
  # do nothing
  x
}

#' @export
drop_unused_value_labels.haven_labelled <- function(x) {
  vl <- val_labels(x)
  val_labels(x) <- vl[vl %in% unique(x)]
  x
}


#' @export
drop_unused_value_labels.data.frame <- function(x) {
  x[] <- lapply(x, drop_unused_value_labels)
  x
}


