#' Get / Set a variable label
#'
#' @param x A vector.
#' @param value A character string or \code{NULL} to remove the label. For data frames,
#'   it could also be a named list.
#' @details
#'   For data frames, if \code{value} is a named list, only elements whose name will
#'   match a column of the data frame will be taken into account.
#' @examples
#' var_label(iris$Sepal.Length)
#' var_label(iris$Sepal.Length) <- 'Length of the sepal'
#' \dontrun{
#'  View(iris)
#' }
#' # To remove a variable label
#' var_label(iris$Sepal.Length) <- NULL
#' @export
var_label <- function(x) {
  UseMethod("var_label")
}

#' @rdname var_label
#' @export
var_label.default <- function(x) {
  if (!is.atomic(x))
    stop("`x` should be atomic", call. = FALSE, domain = "R-labelled")
  attr(x, "label", exact = TRUE)
}

#' @rdname var_label
#' @export
var_label.data.frame <- function(x) {
  lapply(x, var_label)
}

#' @rdname var_label
#' @export
`var_label<-` <- function(x, value) {
  UseMethod("var_label<-")
}

#' @rdname var_label
#' @export
`var_label<-.default` <- function(x, value) {
  if (!is.atomic(x))
    stop("`x` should be atomic", call. = FALSE, domain = "R-labelled")
  if ((!is.character(value) & !is.null(value)) | length(value) >
    1)
    stop("`value` should be a single character string or NULL",
      call. = FALSE, domain = "R-labelled")
  attr(x, "label") <- value
  x
}

#' @rdname var_label
#' @export
`var_label<-.data.frame` <- function(x, value) {
  if ((!is.character(value) & !is.null(value)) & !is.list(value) |
    (is.character(value) & length(value) > 1))
    stop("`value` should be a single character string, NULL or a named list",
      call. = FALSE, domain = "R-labelled")
  if (is.character(value)) {
    value <- as.list(rep(value, ncol(x)))
    names(value) <- names(x)
  }
  if (is.null(value)) {
    value <- as.list(rep(1, ncol(x)))
    names(value) <- names(x)
    value <- lapply(value, function(x) {
      x <- NULL
    })
  }
  value <- value[names(value) %in% names(x)]
  for (var in names(value)) var_label(x[[var]]) <- value[[var]]
  x
}
