#' Get / Set value labels
#'
#' @param x A vector.
#' @param prefixed Should labels be prefixed with values?
#' @param v A single value.
#' @param value A named vector for `val_labels()` (see [haven::labelled()]) or a character string
#'   for `val_labels()`. `NULL` to remove the labels.
#'   For data frames, it could also be a named list with a vector of value labels per variable.
#' @return
#'   `val_labels()` will return a named vector.
#'   `val_label()` will return a single character string.
#' @examples
#' v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9))
#' val_labels(v)
#' val_labels(v, prefixed = TRUE)
#' val_label(v, 2)
#' val_label(v, 2) <- 'maybe'
#' val_label(v, 9) <- NULL
#' val_labels(v) <- NULL
#' @export
val_labels <- function(x, prefixed = FALSE) {
  UseMethod("val_labels")
}

#' @rdname val_labels
#' @export
val_labels.default <- function(x, prefixed = FALSE) {
  # return nothing
  NULL
}

#' @rdname val_labels
#' @export
val_labels.haven_labelled <- function(x, prefixed = FALSE) {
  labels <- attr(x, "labels", exact = TRUE)
  if (prefixed)
    names(labels) <- paste0("[", labels, "] ", names(labels))
  labels
}

#' @rdname val_labels
#' @export
val_labels.data.frame <- function(x, prefixed = FALSE) {
  lapply(x, val_labels, prefixed = prefixed)
}

#' @rdname val_labels
#' @export
`val_labels<-` <- function(x, value) {
  UseMethod("val_labels<-")
}

#' @export
`val_labels<-.default` <- function(x, value) {
  # do nothing
  x
}

#' @rdname val_labels
#' @export
`val_labels<-.numeric` <- function(x, value) {
  if (!is.null(value)) {
    x <- labelled(x, value, label = var_label(x))
  }
  x
}

#' @rdname val_labels
#' @export
`val_labels<-.character` <- function(x, value) {
  if (!is.null(value)) {
    x <- labelled(x, value, label = var_label(x))
  }
  x
}

#' @rdname val_labels
#' @export
`val_labels<-.haven_labelled` <- function(x, value) {
  if (is.null(value)) {
    x <- unclass(x)
    attr(x, "labels") <- NULL
  } else {
    x <- labelled(x, value, label = var_label(x))
  }
  x
}

#' @rdname val_labels
#' @export
`val_labels<-.haven_labelled_spss` <- function(x, value) {
  if (is.null(value) & is.null(attr(x, "na_values")) & is.null(attr(x, "na_range"))) {
    x <- unclass(x)
    attr(x, "labels") <- NULL
  } else {
    x <- labelled_spss(x, value, na_values = attr(x, "na_values"), na_range = attr(x, "na_range"), label = var_label(x))
  }
  x
}

#' @rdname val_labels
#' @export
`val_labels<-.data.frame` <- function(x, value) {
  if (!is.list(value)) {
    temp <- as.list(rep(1, ncol(x)))
    names(temp) <- names(x)
    value <- lapply(temp, function(x) {
      x <- value
    })
  }

  value <- value[names(value) %in% names(x)]

  for (var in names(value)) if (!is.null(value[[var]])) {
    if (mode(x[[var]]) != mode(value[[var]]))
      stop("`x` and `value` must be same type", call. = FALSE,
        domain = "R-labelled")
    if (typeof(x[[var]]) != typeof(value[[var]]))
      mode(value[[var]]) <- typeof(x[[var]])
  }

  for (var in names(value)) val_labels(x[[var]]) <- value[[var]]

  x
}


#' @rdname val_labels
#' @export
val_label <- function(x, v, prefixed = FALSE) {
  UseMethod("val_label")
}

#' @export
val_label.default <- function(x, v, prefixed = FALSE) {
  if (length(v) != 1)
    stop("`v` should be a single value", call. = FALSE, domain = "R-labelled")
  # return nothing
  NULL
}

#' @rdname val_labels
#' @export
val_label.haven_labelled <- function(x, v, prefixed = FALSE) {
  if (length(v) != 1)
    stop("`v` should be a single value", call. = FALSE, domain = "R-labelled")
  labels <- val_labels(x)
  if (v %in% labels) {
    if (prefixed) {
      paste0("[", v, "] ", names(labels)[labels == v])
    } else {
      names(labels)[labels == v]
    }
  } else {
    NULL
  }
}

#' @rdname val_labels
#' @export
val_label.data.frame <- function(x, v, prefixed = FALSE) {
  lapply(x, val_label, v = v, prefixed = prefixed)
}

#' @rdname val_labels
#' @export
`val_label<-` <- function(x, v, value) {
  UseMethod("val_label<-")
}

#' @rdname val_labels
#' @export
`val_label<-.haven_labelled` <- function(x, v, value) {
  if (length(v) != 1)
    stop("`v` should be a single value", call. = FALSE, domain = "R-labelled")
  if (length(value) > 1)
    stop("`value` should be a single character string or NULL",
      call. = FALSE, domain = "R-labelled")

  labels <- val_labels(x)

  if (is.null(value)) {
    if (v %in% labels)
      labels <- labels[labels != v]
  } else {
    if (v %in% labels) {
      names(labels)[labels == v] <- value
    } else {
      names(v) <- value
      labels <- c(labels, v)
    }
  }

  if (length(labels) == 0)
    labels <- NULL

  val_labels(x) <- labels
  x
}

#' @rdname val_labels
#' @export
`val_label<-.numeric` <- function(x, v, value) {
  `val_label<-.haven_labelled`(x = x, v = v, value = value)
}

#' @rdname val_labels
#' @export
`val_label<-.character` <- function(x, v, value) {
  `val_label<-.haven_labelled`(x = x, v = v, value = value)
}

#' @rdname val_labels
#' @export
`val_label<-.data.frame` <- function(x, v, value) {
  if (!is.list(value)) {
    temp <- as.list(rep(1, ncol(x)))
    names(temp) <- names(x)
    value <- lapply(temp, function(x) {
      x <- value
    })
  }

  value <- value[names(value) %in% names(x)]

  for (var in names(value)[]) {
    if (!is.character(value[[var]]) & !is.null(value[[var]]))
      stop("`value` should contain only characters or NULL",
        call. = FALSE, domain = "R-labelled")
    if (length(value[[var]]) > 1)
      stop("`value` should contain only one string (or NULL) per variable",
        call. = FALSE, domain = "R-labelled")
  }

  for (var in names(value)) val_label(x[[var]], v) <- value[[var]]

  x
}


#' @rdname val_labels
#' @param .data a data frame
#' @param ... name-value pairs of value labels (see examples)
#' @param .labels value labels to be applied to the data.frame,
#'   using the same syntax as `value` in `val_labels(df) <- value`.
#' @note
#' `set_value_labels()`, `add_value_labels()` and `remove_value_labels()`
#' could be used with \pkg{dplyr} syntax.
#' While `set_value_labels()` will replace the list of value labels,
#' `add_value_labels()` and `remove_value_labels()` will update that list (see examples).
#' @return
#'  `set_value_labels()`, `add_value_labels()` and `remove_value_labels()` will return an updated
#'  copy of `.data`.
#' @examples
#' if (require(dplyr)) {
#'   # setting value labels
#'   df <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>%
#'     set_value_labels(s1 = c(Male = "M", Female = "F"), s2 = c(Yes = 1, No = 2))
#'   val_labels(df)
#'
#'   # updating value labels
#'   df <- df %>% add_value_labels(s2 = c(Unknown = 9))
#'   df$s2
#'
#'   # removing a value labels
#'   df <- df %>% remove_value_labels(s2 = 9)
#'   df$s2
#'
#'   # removing all value labels
#'   df <- df %>% set_value_labels(s2 = NULL)
#'   df$s2
#' }
#' @export
set_value_labels <- function(.data, ..., .labels = NA) {
  if (!identical(.labels, NA)) {
    val_labels(.data) <- .labels
  }
  values <- rlang::dots_list(...)
  if (!all(names(values) %in% names(.data)))
    stop("some variables not found in .data")

  for (v in names(values))
    val_labels(.data[[v]]) <- values[[v]]

  .data
}

#' @rdname val_labels
#' @export
add_value_labels <- function(.data, ...) {
  values <- rlang::dots_list(...)
  if (!all(names(values) %in% names(.data)))
    stop("some variables not found in .data")

  for(v in values)
    if (is.null(names(v)) | any(names(v) == ""))
      stop("all arguments should be named vectors")

  for (v in names(values))
    for (l in names(values[[v]]))
      val_label(.data[[v]], values[[v]][[l]]) <- l

  .data
}

#' @rdname val_labels
#' @export
remove_value_labels <- function(.data, ...) {
  values <- rlang::dots_list(...)
  if (!all(names(values) %in% names(.data)))
    stop("some variables not found in .data")

  for (v in names(values))
    for (l in values[[v]])
      val_label(.data[[v]], l) <- NULL

    .data
}

#' Sort value labels
#'
#' Sort value labels according to values or to labels
#'
#' @param x A labelled vector.
#' @param according_to According to values or to labels?
#' @param decreasing In decreasing order?
#' @examples
#' v <- labelled(c(1, 2, 3), c(maybe = 2, yes = 1, no = 3))
#' v
#' sort_val_labels(v)
#' sort_val_labels(v, decreasing = TRUE)
#' sort_val_labels(v, 'l')
#' sort_val_labels(v, 'l', TRUE)
#' @export
sort_val_labels <- function(x, according_to = c("values", "labels"),
  decreasing = FALSE) {
  UseMethod("sort_val_labels")
}

#' @export
sort_val_labels.default <- function(x, according_to = c("values",
  "labels"), decreasing = FALSE) {
  # do nothing
  x
}

#' @rdname sort_val_labels
#' @export
sort_val_labels.haven_labelled <- function(x, according_to = c("values",
  "labels"), decreasing = FALSE) {
  according_to <- match.arg(according_to)
  labels <- val_labels(x)
  if (!is.null(labels)) {
    if (according_to == "values")
      labels <- sort(labels, decreasing = decreasing)
    if (according_to == "labels")
      labels <- labels[order(names(labels), decreasing = decreasing)]
    val_labels(x) <- labels
  }
  x
}

#' @rdname sort_val_labels
#' @export
sort_val_labels.data.frame <- function(x, according_to = c("values",
  "labels"), decreasing = FALSE) {
  x[] <- lapply(x, sort_val_labels, according_to = according_to,
    decreasing = decreasing)
  x
}

#' Turn a named vector into a vector of names prefixed by values
#' @param x vector to be prefixed
#' @examples
#' df <- dplyr::tibble(
#'   c1 = labelled(c("M", "M", "F"), c(Male = "M", Female = "F")),
#'   c2 = labelled(c(1, 1, 2), c(Yes = 1, No = 2))
#' )
#' val_labels(df$c1)
#' val_labels(df$c1) %>% names_prefixed_by_values()
#' val_labels(df)
#' val_labels(df) %>% names_prefixed_by_values()
#' @export
names_prefixed_by_values <- function(x) {
  UseMethod("names_prefixed_by_values")
}

#' @rdname names_prefixed_by_values
#' @export
names_prefixed_by_values.default <- function(x) {
  if (is.null(x)) return(NULL)
  paste0("[", x, "] ", names(x))
}

#' @rdname names_prefixed_by_values
#' @export
names_prefixed_by_values.list <- function(x) {
  lapply(x, names_prefixed_by_values)
}
