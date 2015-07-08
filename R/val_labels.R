#' Get / Set value labels
#'
#' @param x A vector.
#' @param v A single value.
#' @param value A named vector for \code{val_labels} (see \code{\link{labelled}}) or a character string
#'   for \code{\link{val_labels}}. \code{NULL} to remove the labels.
#' @param quiet Hide messages?
#' @return
#'   \code{val_labels} will return a named vector.
#'   \code{val_label} will return a single character string.
#' @examples
#' v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9))
#' val_labels(v)
#' val_label(v, 2)
#' val_label(v, 2) <- "maybe"
#' val_label(v, 9) <- NULL
#' val_labels(v) <- NULL
#' @export
val_labels <- function(x) {
  attr(x, "labels", exact = TRUE)
}

#' @rdname val_labels
#' @export
`val_labels<-` <- function(x, value, quiet = FALSE) {
  if (is.null(value)) {
    if (is.labelled(x)) {
      x <- unclass(x)
      attr(x, "labels") <- NULL
      attr(x, "is_na") <- NULL
      if (!quiet)
        message("`x` is not anymore a labelled variable", domain = "R-labelled")
    }
  } else {
    if (!is.labelled(x)) {
      x <- labelled(x, value)
      if (!quiet)
        message("`x` has been converted to a labelled variable", domain = "R-labelled")
    } else {
      if (typeof(value) != typeof(x))
        stop("`x` and `value` must be same type", call. = FALSE, domain = "R-labelled")
      if (is.null(names(value)))
        stop("`value` must be a named vector", call. = FALSE, domain = "R-labelled")
      if (length(value) != length(unique(value)))
        stop("each value should be unique", call. = FALSE, domain = "R-labelled")
      if (any(!missing_val(x) %in% value)) {
        rm_missing_val <- missing_val(x)[!missing_val(x) %in% value]
        if (!quiet)
          message(
            gettextf(
              "some missing values (%s) have been removed",
              paste(rm_missing_val, collapse = ", "),
              domain = "R-labelled"
            )
          )
      }
      attr(x, "is_na") <- value %in% missing_val(x) # changing is_na before labels
      attr(x, "labels") <- value
    }
  }
  x
}

#' @rdname val_labels
#' @export
val_label <- function(x, v) {
  if (length(v)!=1)
    stop("`v` should be a single value", call. = FALSE, domain = "R-labelled")
  labels <- val_labels(x)
  if (v %in% labels)
    names(labels)[labels == v]
  else
    NULL
}

#' @rdname val_labels
#' @export
`val_label<-` <- function(x, v, value, quiet = FALSE) {
  if (length(v)!=1)
    stop("`v` should be a single value", call. = FALSE, domain = "R-labelled")
  if (length(value)>1)
    stop("`value` should be a single character string or NULL", call. = FALSE, domain = "R-labelled")

  labels <- val_labels(x)

  if (is.null(value)) {
    if (v %in% labels)
      labels <- labels[labels != v]
  } else {
    if (v %in% labels) {
      names(labels)[labels == v] <- value
    } else {
      names(v) <- value
      labels <- c(labels, v)
    }
  }

  if (length(labels) == 0)
    labels <- NULL

  val_labels(x, quiet = quiet) <- labels
  x
}
