#' Convert input to a character vector
#'
#' By default, `to_character()` is a wrapper for [base::as.character()].
#' For labelled vector, to_character allows to specify if value, labels or labels prefixed
#' with values should be used for conversion.
#'
#' @param x Object to coerce to a character vector.
#' @param ... Other arguments passed down to method.
#' @export
to_character <- function(x, ...) {
  UseMethod("to_character")
}

#' @export
to_character.default <- function(x, ...) {
  vl <- var_label(x)
  x <- as.character(x)
  var_label(x) <- vl
  x
}

#' @rdname to_character
#' @param levels What should be used for the factor levels: the labels, the values or labels prefixed with values?
#' @param nolabel_to_na Should values with no label be converted to `NA`?
#' @param user_na_to_na user defined missing values into NA?
#' @details
#'   If some values doesn't have a label, automatic labels will be created, except if
#'   `nolabel_to_na` is `TRUE`.
#' @examples
#' v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9))
#' to_character(v)
#' to_character(v, nolabel_to_na = TRUE)
#' to_character(v, "v")
#' to_character(v, "p")
#' @export
to_character.haven_labelled <- function(x, levels = c("labels", "values",
  "prefixed"), nolabel_to_na = FALSE, user_na_to_na = FALSE, ...) {
  vl <- var_label(x)
  levels <- match.arg(levels)
  x <- as.character(to_factor(x, levels = levels, nolabel_to_na = nolabel_to_na, user_na_to_na = user_na_to_na))
  var_label(x) <- vl
  x
}
