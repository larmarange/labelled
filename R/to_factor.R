#' Convert input to a factor.
#'
#' The base function \code{\link[base:factor]{base::as.factor()}} is not a generic, but this variant
#' is. By default, \code{to_factor} is a wrapper for \code{\link[base:factor]{base::as.factor()}}.
#' Please note that \code{to_factor} differs slightly from \code{\link[haven]{as_factor}}
#' method provided by \code{haven} package.
#'
#' @param x Object to coerce to a factor.
#' @param ... Other arguments passed down to method.
#' @export
to_factor <- function(x, ...) {
  UseMethod("to_factor")
}

#' @export
to_factor.factor <- function(x, ...) {
  x
}

#' @export
to_factor.default <- function(x, ...) {
  vl <- var_label(x)
  x <- as.factor(x)
  var_label(x) <- vl
  x
}

#' @rdname to_factor
#' @param levels What should be used for the factor levels: the labels, the values or labels prefixed with values?
#' @param ordered \code{TRUE} for ordinal factors, \code{FALSE} (default) for nominal factors.
#' @param nolabel_to_na Should values with no label be converted to `NA`?
#' @param sort_levels How the factor levels should be sorted? (see Details)
#' @param decreasing Should levels be sorted in decreasing order?
#' @param drop_unused_labels Should unused value labels be dropped?
#'   (applied only if \code{strict = FALSE})
#' @param user_na_to_na Convert user defined missing values into \code{NA}?
#' @param strict Convert to factor only if all values have a defined label?
#' @param unclass If not converted to a factor (when \code{strict = TRUE}),
#'   convert to a character or a numeric factor?
#' @details
#'   If some values doesn't have a label, automatic labels will be created, except if
#'   \code{nolabel_to_na} is \code{TRUE}.
#'
#'   If \code{sort_levels == 'values'}, the levels will be sorted according to the values of \code{x}.
#'   If \code{sort_levels == 'labels'}, the levels will be sorted according to labels' names.
#'   If \code{sort_levels == 'none'}, the levels will be in the order the value labels are defined
#'   in \code{x}. If some labels are automatically created, they will be added at the end.
#'   If \code{sort_levels == 'auto'}, \code{sort_levels == 'none'} will be used, except if some
#'   values doesn't have a defined label. In such case,  \code{sort_levels == 'values'} will
#'   be applied.
#' @examples
#' v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9))
#' to_factor(v)
#' to_factor(v, nolabel_to_na = TRUE)
#' to_factor(v, 'p')
#' to_factor(v, sort_levels = 'v')
#' to_factor(v, sort_levels = 'n')
#' to_factor(v, sort_levels = 'l')
#'
#' x <- labelled(c('H', 'M', 'H', 'L'), c(low = 'L', medium = 'M', high = 'H'))
#' to_factor(x, ordered = TRUE)
#'
#' # Strict conversion
#' v <- labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2))
#' to_factor(v)
#' to_factor(v, strict = TRUE) # Not converted because 3 does not have a label
#' to_factor(v, strict = TRUE, unclass = TRUE)
#' @export
to_factor.haven_labelled <- function(x, levels = c("labels", "values",
  "prefixed"), ordered = FALSE, nolabel_to_na = FALSE,
  sort_levels = c("auto", "none", "labels", "values"), decreasing = FALSE,
  drop_unused_labels = FALSE, user_na_to_na = FALSE, strict = FALSE,
  unclass = FALSE,
  ...) {
  vl <- var_label(x)
  levels <- match.arg(levels)
  sort_levels <- match.arg(sort_levels)
  if (user_na_to_na)
    x <- user_na_to_na(x)
  if (strict) {
    allval <- unique(x)
    allval <- allval[!is.na(allval)]
    nolabel <- allval[!allval %in% val_labels(x)]
    if (length(nolabel) > 0) {
      if (unclass) {
        na_values(x) <- NULL
        na_range(x) <- NULL
        val_labels(x) <- NULL
      }
      return(x)
    }
  }
  if (nolabel_to_na)
    x <- nolabel_to_na(x)
  labels <- val_labels(x)
  allval <- unique(x)
  allval <- allval[!is.na(allval)]
  nolabel <- sort(allval[!allval %in% labels])
  # if there are some values with no label
  if (length(nolabel) > 0) {
    names(nolabel) <- nolabel
    levs <- c(labels, nolabel)
  } else {
    levs <- labels
  }

  if (sort_levels == "auto" & length(nolabel) > 0)
    sort_levels <- "values"
  if (sort_levels == "labels")
    levs <- levs[order(names(levs), decreasing = decreasing)]
  if (sort_levels == "values")
    levs <- sort(levs, decreasing = decreasing)

  if (levels == "labels")
    labs <- names(levs)
  if (levels == "values")
    labs <- unname(levs)
  if (levels == "prefixed")
    labs <- paste0("[", levs, "] ", names(levs))
  levs <- unname(levs)
  x <- factor(x, levels = levs, labels = labs, ordered = ordered,
    ...)
  if (drop_unused_labels)
    x <- droplevels(x)
  var_label(x) <- vl
  x
}

#' @rdname to_factor
#' @param labelled_only for a data.frame, convert only labelled variables to factors?
#' @details
#'   When applied to a data.frame, only labelled vectors are converted by default to a
#'   factor. Use \code{labelled_only = FALSE} to convert all variables to factors.
#' @export
to_factor.data.frame <- function(x, levels = c("labels", "values", "prefixed"),
                                 ordered = FALSE, nolabel_to_na = FALSE,
                                 sort_levels = c("auto", "none", "labels", "values"),
                                 decreasing = FALSE, labelled_only = TRUE,
                                 drop_unused_labels = FALSE, strict = FALSE,
                                 unclass = FALSE,
                                 ...) {
  cl <- class(x)
  x <- dplyr::as_data_frame(lapply(x, .to_factor_col_data_frame, levels = levels, ordered = ordered,
         nolabel_to_na = nolabel_to_na, sort_levels = sort_levels, decreasing = decreasing,
         labelled_only = labelled_only, drop_unused_labels = drop_unused_labels, strict = strict,
         unclass = unclass, ...))
  class(x) <- cl
  x
}

.to_factor_col_data_frame <- function(x, levels = c("labels", "values", "prefixed"),
                                      ordered = FALSE, nolabel_to_na = FALSE,
                                      sort_levels = c("auto", "none", "labels", "values"),
                                      decreasing = FALSE, labelled_only = TRUE,
                                      drop_unused_labels = FALSE, strict = FALSE,
                                      unclass = FALSE,
                                      ...) {
  if (inherits(x, "haven_labelled"))
    x <- to_factor(x, levels = levels, ordered = ordered,
                   nolabel_to_na = nolabel_to_na, sort_levels = sort_levels, decreasing = decreasing,
                   drop_unused_labels = drop_unused_labels, strict = strict, unclass = unclass, ...)
  else if (!labelled_only)
    x <- to_factor(x)
  x
}

