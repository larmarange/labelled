#' Update labelled data to last version
#'
#' Labelled data imported with \pkg{haven} version 1.1.2 or before or
#' created with [haven::labelled()] version 1.1.0 or before was using
#' "labelled" and "labelled_spss" classes.
#'
#' Since version 2.0.0 of these two packages, "haven_labelled" and
#' "haven_labelled_spss" are used instead.
#'
#' Since haven 2.3.0, "haven_labelled" class has been evolving
#' using now \pkg{vctrs} package.
#'
#' `update_labelled()` convert labelled vectors
#' from the old to the new classes and to reconstruct all
#' labelled vectors with the last version of the package.
#'
#' @param x An object (vector or data.frame) to convert.
#' @seealso [haven::labelled()], [haven::labelled_spss()]
#' @export
update_labelled <- function(x) {
  UseMethod("update_labelled")
}

#' @export
update_labelled.default <- function(x) {
  # return x
  x
}

#' @rdname update_labelled
#' @export
update_labelled.labelled <- function(x) {
  # update only previous labelled class, but not objects from Hmisc
  if (!is.null(attr(x, "labels", exact = TRUE))) {
    if (is.null(attr(x, "na_values", exact = TRUE)) & is.null(attr(x, "na_range", exact = TRUE))) {
      x <- labelled(x, labels = attr(x, "labels", exact = TRUE), label = attr(x, "label", exact = TRUE))
    } else {
      x <- labelled_spss(
        x, na_values = attr(x, "na_values", exact = TRUE), na_range = attr(x, "range", exact = TRUE),
        labels = attr(x, "labels", exact = TRUE), label = attr(x, "label", exact = TRUE)
      )
    }
  }
  x
}

#' @rdname update_labelled
#' @export
update_labelled.haven_labelled_spss <- function(x) {
  labelled_spss(
    x, labels = val_labels(x), label = var_label(x),
    na_values = na_values(x), na_range = na_range(x)
  )
}


#' @rdname update_labelled
#' @export
update_labelled.haven_labelled <- function(x) {
  labelled(
    x, labels = val_labels(x), label = var_label(x)
  )
}

#' @rdname update_labelled
#' @export
update_labelled.data.frame <- function(x) {
  x[] <- lapply(x, update_labelled)
  x
}
