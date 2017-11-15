#' Get / Set a variable label
#'
#' @param x An object.
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

#' @export
var_label.default <- function(x) {
  attr(x, "label", exact = TRUE)
}

#' @export
var_label.data.frame <- function(x) {
  lapply(x, var_label)
}

#' @rdname var_label
#' @export
`var_label<-` <- function(x, value) {
  UseMethod("var_label<-")
}

#' @export
`var_label<-.default` <- function(x, value) {
  if ((!is.character(value) & !is.null(value)) | length(value) >
    1)
    stop("`value` should be a single character string or NULL",
      call. = FALSE, domain = "R-labelled")
  attr(x, "label") <- value
  x
}

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


#' @rdname var_label
#' @param .data a data frame
#' @param ... name-value pairs of variable labels (see examples)
#' @note
#'   \code{set_variable_labels} could be used with \code{dplyr}.
#' @return
#'  \code{set_variable_labels} will return an updated copy of \code{.data}.
#' @examples
#' if (require(dplyr)) {
#'   # adding some variable labels
#'   df <- data_frame(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>%
#'     set_variable_labels(s1 = "Sex", s2 = "Yes or No?")
#'   var_label(df)
#'
#'   # removing a variable label
#'   df <- df %>% set_variable_labels(s2 = NULL)
#'   var_label(df$s2)
#' }
#' @export
set_variable_labels <- function(.data, ...) {
  values <- list(...)
  if (!all(names(values) %in% names(.data)))
    stop("some variables not found in .data")

  for (v in names(values))
    var_label(.data[[v]]) <- values[[v]]

  .data
}

