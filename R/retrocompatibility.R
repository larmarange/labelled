#' Update labelled data to version 2.0.0
#'
#' Labelled data imported with \code{haven} version 1.1.2 or before or
#' created with \code{labelled} version 1.1.0 or before was using
#' "labelled" and "labelled_spss" classes.
#'
#' Since version 2.0.0 of these two packages, "haven_labelled" and
#' "haven_labelled_spss" are used instead.
#'
#' \code{update_labelled} convert labelled vectors
#' from the old to the new classes.
#'
#' @param x An object (vector or data.frame) to convert.
#' @export
update_labelled <- function(x) {
  UseMethod("update_labelled")
}

#' @export
update_labelled.default <- function(x) {
  # return nothing
  NULL
}

#' @rdname update_labelled
#' @export
update_labelled.labelled <- function(x) {
  # update only previous labelled class, but not objects from Hmisc
  if (!is.null(attr(x, "labels"))) {
    if (is.null(attr(x, "na_values")) & is.null(attr(x, "na_range"))) {
      x <- labelled(x, labels = attr(x, "labels"), label = attr(x, "label"))
    } else {
      x <- labelled_spss(
        x, na_values = attr(x, "na_values"), na_range = attr(x, "range"),
        labels = attr(x, "labels"), label = attr(x, "label")
      )
    }
  }
  x
}

#' @rdname update_labelled
#' @export
update_labelled.data.frame <- function(x) {
  x[] <- lapply(x, update_labelled)
  x
}
