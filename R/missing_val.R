#' Get / Set a self-defined missing value
#'
#' @param x A vector.
#' @param value A vector indicating which values should be considered as missing.
#' @param force If required, create missing value labels? (see details)
#' @param quiet Hide messages?
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
  miss <-
    attr(x, "labels", exact = TRUE)[attr(x, "is_na", exact = TRUE)]
  if (length(miss) > 0)
    miss
  else
    NULL
}

#' @rdname missing_val
#' @export
`missing_val<-` <- function(x, value, force = NULL, quiet = TRUE) {
  labels <- val_labels(x)
  if (is.null(value)) {
    if (length(labels) > 0)
      attr(x, "is_na") <- rep(FALSE, length(labels))
    else
      attr(x, "is_na") <- NULL
  } else {
    if (typeof(value) != typeof(x))
      stop("`x` and `value` must be same type", call. = FALSE, domain = "R-labelled")
    nolabel <- value[!value %in% labels]
    if (length(nolabel) > 0) {
      if (is.null(force)) {
        stop(
          gettextf(
            "no value label found for %s, please specify `force`", paste(nolabel, sep = ", "),
            domain = "R-labelled"
          ), call. = FALSE
        )
      }
      else {
        if (force) {
          for (nl in nolabel) {
            val_label(x, nl, quiet = quiet) <- nl
          }
          if (!quiet)
            message(
              gettextf(
                "some value label (%s) have been created", paste(nolabel, sep = ", "),
                domain = "R-labelled"
              )
            )
        } else {
          value <- value[value %in% labels]
          if (!quiet)
            message(
              gettextf(
                "no value label found for %s, no missing value defined", paste(nolabel, sep = ", "),
                domain = "R-labelled"
              )
            )
        }
      }
    }
    # using val_labels() as labels could be out of date
    attr(x, "is_na") <- val_labels(x) %in% value
  }
  x
}
