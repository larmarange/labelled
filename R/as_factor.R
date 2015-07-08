#' Convert input to a factor.
#'
#' The base function \code{\link[base]{as.factor}} is not a generic, but this variant
#' is. By default, \code{as_factor} is a wrapper for \code{\link[base]{factor}}.
#'
#' @param x Object to coerce to a factor.
#' @param ... Other arguments passed down to method.
#' @export
as_factor <- function(x, ...) {
  UseMethod("as_factor")
}

#' @export
as_factor.factor <- function(x, ...) {
  x
}

#' @rdname as_factor
#' @export
as_factor.default <- function(x, ...) {
  factor(x, ...)
}

#' @rdname as_factor
#' @param levels What should be used for the factor levels: the labels, the values or labels prefixed with values?
#' @param ordered \code{TRUE} for ordinal factors, \code{FALSE} (default) for nominal factors.
#' @param missing_to_na Should defined missing values be converted to `NA`?
#' @param nolabel_to_na Should values with no label be converted to `NA`?
#' @param quiet Hide messages?
#' @details The order of the returned factor levels will be the order of value labels, except if some values doesn't have
#'   a label and \code{nolabel_to_na} is \code{FALSE}. In such case, automatic labels will be created and levels will be
#'   ordered according to values. To order labels before calling \code{as_factor}, see \code{\link{sort_labels}}.
#' @examples
#' v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9), c(F, F, T))
#' as_factor(v)
#' as_factor(v, missing_to_na = FALSE, nolabel_to_na = TRUE)
#' as_factor(v, "p")
#'
#' x <- labelled(c("H", "M", "H", "L"), c(low = "L", medium = "M", high = "H"))
#' as_factor(x, ordered = TRUE)
#' @export
as_factor.labelled <-
  function(x, levels = c("labels", "values", "prefixed"),
           ordered = FALSE, missing_to_na = TRUE, nolabel_to_na = FALSE, quiet = FALSE, ...) {
    levels <- match.arg(levels)
    if (missing_to_na)
      x <- missing_to_na(x)
    if (nolabel_to_na)
      x <- nolabel_to_na(x)
    labels <- val_labels(x)
    allval <- unique(x)
    allval <- allval[!is.na(allval)]
    nolabel <- allval[!allval %in% labels]
    # if there are some values with no label
    if (length(nolabel) > 0) {
      names(nolabel) <- nolabel
      levs <- c(labels, nolabel)
      levs <- sort(levs)
      if (!quiet & levels != "values")
        message(
          gettextf(
            "no label found for some values (%s), automatic labels created",
            paste(nolabel, collapse = ", "),
            domain = "R-labelled"
          )
        )
    } else {
      levs <- labels
    }
    if (levels == "labels")
      labs <- names(levs)
    else if (levels == "values")
      labs <- unname(levs)
    else if (levels == "prefixed")
      labs <- paste0("[", levs, "] ", names(levs))
    levs <- unname(levs)
    factor(x, levels = levs, labels = labs, ordered = ordered, ...)
  }
