#' Get / Set a variable label
#'
#' @param x an object
#' @param value a character string or `NULL` to remove the label
#'  For data frames, it could also be a named list or a character vector
#'  of same length as the number of columns in `x`.
#' @param unlist for data frames, return a named vector instead of a list
#' @details
#'   For data frames, if `value` is a named list, only elements whose name will
#'   match a column of the data frame will be taken into account. If `value`
#'   is a character vector, labels should in the same order as the columns of the
#'   data.frame.
#' @examples
#' var_label(iris$Sepal.Length)
#' var_label(iris$Sepal.Length) <- 'Length of the sepal'
#' \dontrun{
#'  View(iris)
#' }
#' # To remove a variable label
#' var_label(iris$Sepal.Length) <- NULL
#' # To change several variable labels at once
#' var_label(iris) <- c(
#'   "sepal length", "sepal width", "petal length",
#'   "petal width", "species"
#')
#' var_label(iris)
#' var_label(iris) <- list(
#'   Petal.Width = "width of the petal",
#'   Petal.Length = "length of the petal"
#' )
#' var_label(iris)
#' var_label(iris, unlist = TRUE)
#' @export
var_label <- function(x, unlist = FALSE) {
  UseMethod("var_label")
}

#' @export
var_label.default <- function(x, unlist = FALSE) {
  attr(x, "label", exact = TRUE)
}

#' @export
var_label.data.frame <- function(x, unlist = FALSE) {
  r <- lapply(x, var_label)
  if (unlist) {
    r <- lapply(r, function(x){if (is.null(x)) "" else x})
    base::unlist(r, use.names = TRUE)
  } else
    r
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
    (is.character(value) & length(value) > 1 & length(value) != ncol(x)))
    stop("`value` should be a named list, NULL, a single character string or a character vector of same length than the number of columns in `x`",
      call. = FALSE, domain = "R-labelled")
  if (is.character(value) & length(value) == 1) {
    value <- as.list(rep(value, ncol(x)))
    names(value) <- names(x)
  }
  if (is.character(value) & length(value) == ncol(x)) {
    value <- as.list(value)
    names(value) <- names(x)
  }
  if (is.null(value)) {
    value <- as.list(rep(1, ncol(x)))
    names(value) <- names(x)
    value <- lapply(value, function(x) {
      x <- NULL
    })
  }

  if (!all(names(value) %in% names(x)))
    stop("some variables not found in x")

  value <- value[names(value) %in% names(x)]
  for (var in names(value)) var_label(x[[var]]) <- value[[var]]
  x
}


#' @rdname var_label
#' @param .data a data frame
#' @param ... name-value pairs of variable labels (see examples)
#' @param .labels variable labels to be applied to the data.frame,
#'   using the same syntax as `value` in `var_label(df) <- value`.
#' @note
#'   `set_variable_labels()` could be used with \pkg{dplyr} syntax.
#' @return
#'  `set_variable_labels()` will return an updated copy of `.data`.
#' @examples
#' if (require(dplyr)) {
#'   # adding some variable labels
#'   df <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>%
#'     set_variable_labels(s1 = "Sex", s2 = "Yes or No?")
#'   var_label(df)
#'
#'   # removing a variable label
#'   df <- df %>% set_variable_labels(s2 = NULL)
#'   var_label(df$s2)
#'
#'   # defining variable labels derived from variable names
#'   if (require(snakecase)) {
#'     iris <- iris %>%
#'       set_variable_labels(.labels = to_sentence_case(names(iris)))
#'     var_label(iris)
#'   }
#' }
#' @export
set_variable_labels <- function(.data, ..., .labels = NA) {
  if (!identical(.labels, NA)) {
    var_label(.data) <- .labels
  }
  values <- rlang::dots_list(...)
  if (length(values) > 0) {
    if (!all(names(values) %in% names(.data)))
      stop("some variables not found in .data")

    for (v in names(values))
      var_label(.data[[v]]) <- values[[v]]
  }

  .data
}

