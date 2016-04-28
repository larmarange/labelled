#' Get / Set a self-defined missing value
#'
#' @param x A vector.
#' @param value A vector indicating which values should be considered as missing.
#' @param force If required, create missing value labels? (see details)
#' @details If \code{value} contains some values for whose no value label has been defined,
#'   you will get an error, except if you specify \code{TRUE} or \code{FALSE} to \code{force}.
#'   If \code{TRUE}, the missing value labels will be created. If \code{FALSE}, no missing value
#'   will be defined for these values.
#' @return \code{missing_val} will return a named list of values.
#' @examples
#' v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9))
#' missing_val(v)
#' missing_val(v) <- 9
#' missing_val(v)
#' missing_val(v, FALSE) <- c(8, 9)
#' missing_val(v)
#' missing_val(v, TRUE) <- c(8, 9)
#' missing_val(v)
#' @export
missing_val <- function(x) {
  UseMethod("missing_val")
}

#' @rdname missing_val
#' @export
missing_val.default <- function(x) {
  NULL
}

#' @rdname missing_val
#' @export
missing_val.labelled <- function(x) {
  miss <- attr(x, "labels", exact = TRUE)[attr(x, "is_na",
    exact = TRUE)]
  if (length(miss) > 0) {
    miss
  } else {
    NULL
  }
}

#' @rdname missing_val
#' @export
`missing_val<-` <- function(x, force = NULL, value) {
  UseMethod("missing_val<-")
}

#' @export
`missing_val<-.default` <- function(x, force = NULL, value) {
  # do nothing
  x
}

#' @rdname missing_val
#' @export
`missing_val<-.labelled` <- function(x, force = NULL, value) {
  labels <- val_labels(x)
  if (is.null(value)) {
    if (length(labels) > 0) {
      attr(x, "is_na") <- rep(FALSE, length(labels))
    } else {
      attr(x, "is_na") <- NULL
    }
  } else {
    if (mode(value) != mode(x))
      stop("`x` and `value` must be same type", call. = FALSE,
        domain = "R-labelled")
    if (typeof(value) != typeof(x))
      mode(value) <- typeof(x)
    nolabel <- value[!value %in% labels]
    if (length(nolabel) > 0) {
      if (is.null(force)) {
        stop(gettextf("no value label found for %s, please specify `force`",
          paste(nolabel, sep = ", "), domain = "R-labelled"),
          call. = FALSE)
      } else {
        if (force) {
          for (nl in nolabel) {
          val_label(x, nl) <- nl
          }
        } else {
          value <- value[value %in% labels]
        }
      }
    }
    # using val_labels() as labels could be out of date
    attr(x, "is_na") <- val_labels(x) %in% value
  }
  x
}

#' @rdname missing_val
#' @export
`missing_val<-.numeric` <- function(x, force = NULL, value) {
  `missing_val<-.labelled`(x = x, value = value, force = force)
}

#' @rdname missing_val
#' @export
`missing_val<-.character` <- function(x, force = NULL, value) {
  `missing_val<-.labelled`(x = x, value = value, force = force)
}


#' @rdname missing_val
#' @export
`missing_val<-.data.frame` <- function(x, force, value) {
  if (!is.list(value)) {
    temp <- as.list(rep(1, ncol(x)))
    names(temp) <- names(x)
    value <- lapply(temp, function(x) {
      x <- value
    })
  }

  value <- value[names(value) %in% names(x)]

  for (var in names(value)) if (!is.null(value[[var]]))
    if (typeof(x[[var]]) != typeof(value[[var]]))
      stop("`x` and `value` must be same type", call. = FALSE,
        domain = "R-labelled")

  for (var in names(value)) missing_val(x[[var]], force = force) <- value[[var]]

  x
}

