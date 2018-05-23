#' Copy variable and value labels
#'
#' This function copies variable and value labels (including missing values)
#' from one vector to another or from one data frame to another data frame.
#' For data frame, labels are copied according to variable names, and only
#' if variables are the same type in both data frames.
#'
#' Some base \R functions like \code{\link[base]{subset}} drop variable and
#' value labels attached to a variable. \code{copy_labels} coud be used
#' to restore these attributes.
#'
#' @param from Object to copy labels from.
#' @param to Object to copu labels to.
#' @export
copy_labels <- function(from, to) {
  UseMethod("copy_labels")
}


#' @export
copy_labels.default <- function(from, to) {
  if (!is.atomic(from) | !is.atomic(to))
    stop("`from` and `to` should be vectors", call. = FALSE,
      domain = "R-labelled")
  if (mode(from) != mode(to))
    stop("`from` and `to` should be of same type", call. = FALSE,
      domain = "R-labelled")
  var_label(to) <- var_label(from)
  if (!is.factor(to)) {
    val_labels(to) <- val_labels(from)
    na_range(to) <- na_range(from)
    na_values(to) <- na_values(from)
  }
  to
}

#' @export
copy_labels.data.frame <- function(from, to) {
  if (!is.data.frame(to))
    stop("`to` should be a data frame", call. = FALSE, domain = "R-labelled")
  for (var in names(to)) if (var %in% names(from))
    to[[var]] <- copy_labels(from[[var]], to[[var]])
  to
}



