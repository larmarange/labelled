#' Get / Set a variable label
#'
#' @param x a vector or a data.frame
#' @param value a character string or `NULL` to remove the label
#'  For data frames, with `var_label()`, it could also be a named list or a
#'  character vector of same length as the number of columns in `x`.
#' @param unlist for data frames, return a named vector instead of a list
#' @param null_action for data frames, by default `NULL` will be returned for
#' columns with no variable label. Use `"fill"` to populate with the column name
#' instead, `"skip"` to remove such values from the returned list, `"na"` to
#' populate with `NA` or `"empty"` to populate with an empty string (`""`).
#' @param recurse if `TRUE`, will apply `var_label()` on packed columns
#' (see [tidyr::pack()]) to return the variable labels of each sub-column;
#' otherwise, the label of the group of columns will be returned.
#' @details
#'   `get_variable_labels()` is identical to `var_label()`.
#'
#'   For data frames, if you are using `var_label()<-` and if `value` is a
#'   named list, only elements whose name will match a column of the data frame
#'   will be taken into account. If `value` is a character vector, labels should
#'   be in the same order as the columns of the data.frame.
#'
#'   If you are using `label_attribute()<-` or `set_label_attribute()` on a data
#'   frame, the label attribute will be attached to the data frame itself, not
#'   to a column of the data frame.
#'
#'   If you are using packed columns (see [tidyr::pack()]), please read the
#'   dedicated vignette.
#' @examples
#' var_label(iris$Sepal.Length)
#' var_label(iris$Sepal.Length) <- "Length of the sepal"
#' \dontrun{
#' View(iris)
#' }
#' # To remove a variable label
#' var_label(iris$Sepal.Length) <- NULL
#' # To change several variable labels at once
#' var_label(iris) <- c(
#'   "sepal length", "sepal width", "petal length",
#'   "petal width", "species"
#' )
#' var_label(iris)
#' var_label(iris) <- list(
#'   Petal.Width = "width of the petal",
#'   Petal.Length = "length of the petal",
#'   Sepal.Width = NULL,
#'   Sepal.Length = NULL
#' )
#' var_label(iris)
#' var_label(iris, null_action = "fill")
#' var_label(iris, null_action = "skip")
#' var_label(iris, unlist = TRUE)
#'
#' #
#' @export
var_label <- function(x, ...) {
  rlang::check_dots_used()
  UseMethod("var_label")
}

var_label_no_check <- function(x, ...) {
  UseMethod("var_label")
}

#' @export
var_label.default <- function(x, ...) {
  attr(x, "label", exact = TRUE)
}

#' @rdname var_label
#' @export
var_label.data.frame <- function(x,
                                 unlist = FALSE,
                                 null_action =
                                   c("keep", "fill", "skip", "na", "empty"),
                                 recurse = FALSE,
                                 ...) {
  if (recurse) {
    r <- lapply(
      x,
      var_label_no_check,
      unlist = unlist,
      null_action = null_action,
      recurse = TRUE
    )
  } else {
    r <- lapply(x, label_attribute)
  }

  null_action <- match.arg(null_action)

  if (null_action == "fill") {
    r <- mapply(
      function(l, n) {
        if (is.null(l)) n else l
      },
      r,
      names(r),
      SIMPLIFY = FALSE
    )
  }

  if (null_action == "empty") {
    r <- lapply(
      r,
      function(x) {
        if (is.null(x)) "" else x
      }
    )
  }

  if (null_action == "na") {
    r <- lapply(
      r,
      function(x) {
        if (is.null(x)) NA_character_ else x
      }
    )
  }

  if (null_action == "skip") {
    r <- r[!sapply(r, is.null)]
  }

  if (unlist) {
    r <- lapply(
      r,
      function(x) {
        if (is.null(x)) "" else x
      }
    )
    r <- base::unlist(r, use.names = TRUE)
  }

  r
}

#' @rdname var_label
#' @export
`var_label<-` <- function(x, value) {
  UseMethod("var_label<-")
}

#' @export
`var_label<-.default` <- function(x, value) {
  label_attribute(x) <- value
  x
}

#' @export
`var_label<-.data.frame` <- function(x, value) {
  if (
    (!is.character(value) && !is.null(value)) && !is.list(value) ||
      (is.character(value) && length(value) > 1 && length(value) != ncol(x))
  )
    cli::cli_abort(paste(
      "{.arg value} should be a named list, NULL, a single character string",
      "or a character vector of same length than the number of columns",
      "in {.arg x}."
    ))
  if (is.character(value) && length(value) == 1) {
    value <- as.list(rep(value, ncol(x)))
    names(value) <- names(x)
  }
  if (is.character(value) && length(value) == ncol(x)) {
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

  if (!all(names(value) %in% names(x))) {
    missing_names <- setdiff(names(value), names(x))

    cli::cli_abort(c(
      "Can't find variables {.var {missing_names}} in {.arg x}."
    ))
  }

  value <- value[names(value) %in% names(x)]
  for (var in names(value)) label_attribute(x[[var]]) <- value[[var]]
  x
}

#' @rdname var_label
#' @export
get_variable_labels <- var_label


#' @rdname var_label
#' @param .data a data frame or a vector
#' @param ... name-value pairs of variable labels (see examples)
#' @param .labels variable labels to be applied to the data.frame,
#'   using the same syntax as `value` in `var_label(df) <- value`.
#' @param .strict should an error be returned if some labels
#'   doesn't correspond to a column of `x`?
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
#'   # Set labels from dictionary, e.g. as read from external file
#'   # One description is missing, one has no match
#'   description <- tibble(
#'     name = c(
#'       "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
#'       "Something"
#'     ),
#'     label = c(
#'       "Sepal length", "Sepal width", "Petal length", "Petal width",
#'       "something"
#'     )
#'   )
#'   var_labels <- stats::setNames(as.list(description$label), description$name)
#'   iris_labelled <- iris %>%
#'     set_variable_labels(.labels = var_labels, .strict = FALSE)
#'   var_label(iris_labelled)
#'
#'   # defining variable labels derived from variable names
#'   if (require(snakecase)) {
#'     iris <- iris %>%
#'       set_variable_labels(.labels = to_sentence_case(names(iris)))
#'     var_label(iris)
#'   }
#'
#'   # example with a vector
#'   v <- 1:5
#'   v <- v %>% set_variable_labels("a variable label")
#'   v
#'   v %>% set_variable_labels(NULL)
#' }
#' @export
set_variable_labels <- function(.data, ..., .labels = NA, .strict = TRUE) {
  # not a data.frame
  if (!is.data.frame(.data)) {
    if (!identical(.labels, NA)) {
      label_attribute(.data) <- .labels
    } else {
      label_attribute(.data) <- unname(unlist(rlang::dots_list(...)))
    }
    return(.data)
  }

  # data.frame case
  if (!identical(.labels, NA)) {
    if (!.strict) {
      .labels <- .labels[intersect(names(.labels), names(.data))]
    } else {
      check_character(.labels)

    }
    var_label(.data) <- .labels
  }
  values <- rlang::dots_list(...)
  if (length(values) > 0) {
    if (.strict && !all(names(values) %in% names(.data))) {
      missing_names <- setdiff(names(values), names(.data))

      cli::cli_abort(c(
        "Can't find variables {.var {missing_names}}  in {.arg .data}."
      ))
    }

    for (v in intersect(names(values), names(.data))) {
      label_attribute(.data[[v]]) <- values[[v]]
    }
  }

  .data
}

#' @rdname var_label
#' @export
label_attribute <- function(x) {
  attr(x, "label", exact = TRUE)
}

#' @rdname var_label
#' @export
get_label_attribute <- function(x) {
  label_attribute(x)
}

#' @rdname var_label
#' @export
set_label_attribute <- function(x, value) {
  check_string(value, allow_null = TRUE, allow_na = TRUE)
  attr(x, "label") <- value
  x
}

#' @rdname var_label
#' @export
`label_attribute<-` <- set_label_attribute
