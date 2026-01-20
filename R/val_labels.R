#' Get / Set value labels
#'
#' @param x A vector, a data frame or a survey design
#' @param prefixed Should labels be prefixed with values?
#' @param v A single value.
#' @param value A named vector for `val_labels()` (see [haven::labelled()]) or
#'   a character string for `val_label()`. `NULL` to remove the labels (except
#'   if `null_action = "labelled"`).
#'   For data frames and survey design, it could also be a named list with a
#'   vector of value labels per variable.
#' @param null_action,.null_action for advanced users, if `value = NULL`,
#' unclass the vector (default) or force/keep `haven_labelled` class
#' (if `null_action = "labelled"`)
#' @return
#'   `val_labels()` will return a named vector.
#'   `val_label()` will return a single character string.
#' @examples
#' v <- labelled(
#'   c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
#'   c(yes = 1, no = 3, "don't know" = 9)
#' )
#' val_labels(v)
#' val_labels(v, prefixed = TRUE)
#' val_label(v, 2)
#' val_label(v, 2) <- "maybe"
#' v
#' val_label(v, 9) <- NULL
#' v
#' val_labels(v, null_action = "labelled") <- NULL
#' v
#' val_labels(v) <- NULL
#' v
#' @export
val_labels <- function(x, prefixed = FALSE) {
  UseMethod("val_labels")
}

#' @export
val_labels.default <- function(x, prefixed = FALSE) {
  # return nothing
  NULL
}

#' @export
val_labels.haven_labelled <- function(x, prefixed = FALSE) {
  labels <- attr(x, "labels", exact = TRUE)
  if (prefixed) {
    names(labels) <- names_prefixed_by_values(labels)
  }
  labels
}

#' @export
val_labels.data.frame <- function(x, prefixed = FALSE) {
  lapply(x, val_labels, prefixed = prefixed)
}

#' @export
val_labels.survey.design <- function(x, prefixed = FALSE) {
  val_labels(x$variables, prefixed = prefixed)
}

#' @export
val_labels.svyrep.design <- val_labels.survey.design

#' @rdname val_labels
#' @export
`val_labels<-` <- function(
    x,
    null_action = c("unclass", "labelled"),
    value) {
  UseMethod("val_labels<-")
}

#' @export
`val_labels<-.default` <- function(
    x,
    null_action = c("unclass", "labelled"),
    value) {
  null_action <- match.arg(null_action)
  if (!is.null(value) || null_action == "labelled") {
    if (!is.null(value) && all(is.na(x)) && mode(x) != mode(value)) {
      cli::cli_alert_info(
        c(
          "Vectors must be same type. ",
          "i" =
            "Converted {.var x} from {.cls {mode(x)}} to {.cls {mode(value)}}."
        )
      )
      mode(x) <- mode(value)
    }
    x <- labelled(x, value, label = var_label(x))
  }
  # otherwise do nothing
  x
}

#' @export
`val_labels<-.factor` <- function(x,
                                  null_action = c("unclass", "labelled"),
                                  value) {
  null_action <- match.arg(null_action)
  if (!is.null(value) || null_action == "labelled")
    cli::cli_abort("Value labels cannot be applied to factors.")
  x %>% remove_attributes("labels")
}

#' @export
`val_labels<-.numeric` <- function(
  x,
  null_action = c("unclass", "labelled"),
  value
) {
  null_action <- match.arg(null_action)
  if (!is.null(value) && !is.numeric(value))
    cli::cli_abort("{.arg x} and {.arg value} should be same type.")
  if ((!is.null(value) && length(value) > 0) || null_action == "labelled") {
    x <- labelled(x, value, label = var_label(x))
  }
  x
}

#' @export
`val_labels<-.character` <- function(
  x,
  null_action = c("unclass", "labelled"),
  value
) {
  null_action <- match.arg(null_action)
  if (!is.null(value) && !is.character(value))
    cli::cli_abort("{.arg x} and {.arg value} should be same type.")
  if ((!is.null(value) && length(value) > 0) || null_action == "labelled") {
    x <- labelled(x, value, label = var_label(x))
  }
  x
}

#' @export
`val_labels<-.haven_labelled` <- function(
    x,
    null_action = c("unclass", "labelled"),
    value) {
  null_action <- match.arg(null_action)
  if (length(value) == 0) {
    value <- NULL
  }
  if (is.null(value) && null_action == "unclass") {
    x <- unclass(x)
    attr(x, "labels") <- NULL
  } else {
    x <- labelled(x, value, label = var_label(x))
  }
  x
}

#' @export
`val_labels<-.haven_labelled_spss` <- function(
    x,
    null_action = c("unclass", "labelled"),
    value) {
  null_action <- match.arg(null_action)
  if (length(value) == 0) {
    value <- NULL
  }
  if (
    is.null(value) &&
      is.null(attr(x, "na_values")) &&
      is.null(attr(x, "na_range")) &&
      null_action == "unclass"
  ) {
    x <- unclass(x)
    attr(x, "labels") <- NULL
  } else {
    x <-
      labelled_spss(
        x,
        value,
        na_values = attr(x, "na_values"),
        na_range = attr(x, "na_range"),
        label = var_label(x)
      )
  }
  x
}

#' @export
`val_labels<-.data.frame` <- function(
    x,
    null_action = c("unclass", "labelled"),
    value) {
  null_action <- match.arg(null_action)
  if (!is.list(value)) {
    temp <- as.list(rep(1, ncol(x)))
    names(temp) <- names(x)
    value <- lapply(temp, function(x) {
      x <- value
    })
  }

  if (!all(names(value) %in% names(x))) {
    missing_names <- setdiff(names(value), names(x))

    cli::cli_abort(c(
      "Can't find variables {.var {missing_names}} in {.arg x}."
    ))
  }

  for (var in names(value)) {
    if (!is.null(value[[var]])) {
      if (mode(x[[var]]) != mode(value[[var]])) {
        if (all(is.na(x[[var]]))) {
          cli::cli_alert_info(c(
            "Vectors must be same type. ",
            "i" = paste(
              "Converted {.var x${var}} from {.cls {mode(x[[var]])}}",
              "to {.cls {mode(value[[var]])}}."
            )
          ))
          mode(x[[var]]) <- mode(value[[var]])
        } else {
          cli::cli_abort(c(
            "Vectors must be same type. ",
            "x" = paste(
              "{.var x[[{.str {var}}]]} is {.cls {mode(x[[var]])}}",
              "but",
              "{.var value[[{.str {var}}]]} is {.cls {mode(value[[var]])}}."
            )
          ))
        }
      }
      if (typeof(x[[var]]) != typeof(value[[var]])) {
        mode(value[[var]]) <- typeof(x[[var]])
      }
    }
  }

  for (var in names(value)) {
    val_labels(x[[var]], null_action = null_action) <- value[[var]]
  }

  x
}

#' @export
`val_labels<-.survey.design` <- function(
    x,
    null_action = c("unclass", "labelled"),
    value) {

  val_labels(x$variables, null_action = null_action) <- value
  x
}

#' @export
`val_labels<-.svyrep.design` <- `val_labels<-.survey.design`

#' @rdname val_labels
#' @export
val_label <- function(x, v, prefixed = FALSE) {
  UseMethod("val_label")
}

#' @export
val_label.default <- function(x, v, prefixed = FALSE) {
  if (length(v) != 1)
    cli::cli_abort("{.arg v} (length: {length(v)}) should be a single value.")
  # return nothing
  NULL
}

#' @export
val_label.haven_labelled <- function(x, v, prefixed = FALSE) {
  if (length(v) != 1)
    cli::cli_abort("{.arg v} (length: {length(v)}) should be a single value.")
  labels <- val_labels(x, prefixed = prefixed)
  if (v %in% labels) {
    names(labels)[labels == v]
  } else {
    NULL
  }
}

#' @export
val_label.data.frame <- function(x, v, prefixed = FALSE) {
  lapply(x, val_label, v = v, prefixed = prefixed)
}

#' @export
val_label.survey.design <- function(x, v, prefixed = FALSE) {
  val_label(x$variables, v = v, prefixed = prefixed)
}

#' @export
val_label.svyrep.design <- val_label.survey.design

#' @rdname val_labels
#' @export
`val_label<-` <- function(x, v, null_action = c("unclass", "labelled"), value) {
  UseMethod("val_label<-")
}

#' @export
`val_label<-.default` <- function(
    x,
    v,
    null_action = c("unclass", "labelled"),
    value) {
  if (length(v) != 1) {
    cli::cli_abort("{.arg v} (length: {length(v)}) should be a single value.")
  }
  check_string(value, allow_null = TRUE)
  names(value) <- v
  val_labels(x, null_action = null_action) <- value
  x
}

#' @export
`val_label<-.haven_labelled` <- function(
    x,
    v,
    null_action = c("unclass", "labelled"),
    value) {
  if (length(v) != 1) {
    cli::cli_abort("{.arg v} (length: {length(v)}) should be a single value.")
  }
  check_string(value, allow_null = TRUE)

  labels <- val_labels(x)

  if (is.null(value)) {
    if (v %in% labels) {
      labels <- labels[labels != v]
    }
  } else {
    if (v %in% labels) {
      names(labels)[labels == v] <- value
    } else {
      names(v) <- value
      labels <- c(labels, v)
    }
  }

  if (length(labels) == 0) {
    labels <- NULL
  }

  val_labels(x, null_action = null_action) <- labels
  x
}

#' @export
`val_label<-.numeric` <- `val_label<-.haven_labelled`

#' @export
`val_label<-.character` <- `val_label<-.haven_labelled`

#' @export
`val_label<-.data.frame` <- function(
    x,
    v,
    null_action = c("unclass", "labelled"),
    value) {
  if (!is.list(value)) {
    temp <- as.list(rep(1, ncol(x)))
    names(temp) <- names(x)
    value <- lapply(temp, function(x) {
      x <- value
    })
  }

  value <- value[names(value) %in% names(x)]

  for (var in names(value)[]) {
    check_string(value[[var]], allow_null = TRUE, arg = "value")
  }

  for (var in names(value)) {
    val_label(x[[var]], v, null_action = null_action) <- value[[var]]
  }

  x
}

#' @export
`val_label<-.survey.design` <- function(
    x,
    v,
    null_action = c("unclass", "labelled"),
    value) {
  val_label(x$variables, v = v, null_action = null_action) <- value
  x
}

#' @export
`val_label<-.svyrep.design` <- `val_label<-.survey.design`

#' @rdname val_labels
#' @export
get_value_labels <- val_labels

#' @rdname val_labels
#' @param .data a data frame or a vector
#' @param ... name-value pairs of value labels (see examples)
#' @param .labels value labels to be applied to the data.frame,
#'   using the same syntax as `value` in `val_labels(df) <- value`.
#' @param .strict should an error be returned if some labels
#'   doesn't correspond to a column of `x`?
#' @param .overwrite If value labels are already defined, overwrite them?
#' @note
#' `get_value_labels()` is identical to `val_labels()`.
#'
#' `set_value_labels()`, `add_value_labels()` and `remove_value_labels()`
#' could be used with \pkg{dplyr} syntax.
#' While `set_value_labels()` will replace the list of value labels,
#' `add_value_labels()` and `remove_value_labels()` will update that list
#' (see examples).
#'
#' `set_value_labels()` could also be applied to a vector / a data.frame column.
#' In such case, you can provide a vector of value labels using `.labels` or
#' several name-value pairs of value labels (see example).  You
#' could consider [dictionary_to_value_labels()] to generate a list of
#' variable labels from a dictionary data frame.
#' Similarly, `add_value_labels()` and `remove_value_labels()` could also be
#' applied on vectors.
#' @return
#'  `set_value_labels()`, `add_value_labels()` and `remove_value_labels()` will
#'  return an updated copy of `.data`.
#' @examples
#' if (require(dplyr)) {
#'   # setting value labels
#'   df <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>%
#'     set_value_labels(
#'       s1 = c(Male = "M", Female = "F"),
#'       s2 = c(Yes = 1, No = 2)
#'     )
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
#'
#'   # example on a vector
#'   v <- 1:4
#'   v <- set_value_labels(v, min = 1, max = 4)
#'   v
#'   v %>% set_value_labels(middle = 3)
#'   v %>% set_value_labels(NULL)
#'   v %>% set_value_labels(.labels = c(a = 1, b = 2, c = 3, d = 4))
#'   v %>% add_value_labels(between = 2)
#'   v %>% remove_value_labels(4)
#' }
#' @export
set_value_labels <- function(
    .data,
    ...,
    .labels = NA,
    .strict = TRUE,
    .overwrite = TRUE,
    .null_action = c("unclass", "labelled")) {
  .null_action <- match.arg(.null_action)
  # survey.design case
  if (inherits(.data, "survey.design") || inherits(.data, "svyrep.design")) {
    .data$variables <-
      set_value_labels(
        .data$variables,
        ...,
        .labels = .labels,
        .strict = .strict,
        .overwrite = .overwrite,
        .null_action = .null_action
      )
    return(.data)
  }

  if (!is.data.frame(.data) && !is.atomic(.data))
    cli::cli_abort(
      "{.arg .data} should be a data frame, a survey design or a vector."
    )

  # vector case
  if (is.atomic(.data)) {
    if (!.overwrite && !is.null(val_labels(.data))) return(.data)
    if (!identical(.labels, NA)) {
      val_labels(.data, null_action = .null_action) <- .labels
    } else {
      values <- unlist(rlang::dots_list(...))
      val_labels(.data, null_action = .null_action) <- values
    }
    return(.data)
  }

  already <- !unlist(lapply(val_labels(.data), is.null))
  already <- names(already)[already]
  # data.frame case
  if (!identical(.labels, NA)) {
    if (!.strict) {
      .labels <- .labels[intersect(names(.labels), names(.data))]
    }
    if (!.overwrite) {
      .labels <- .labels[setdiff(already, names(.data))]
    }
    val_labels(.data, null_action = .null_action) <- .labels
  }
  values <- rlang::dots_list(...)
  if (.strict && !all(names(values) %in% names(.data))) {
    missing_names <- setdiff(names(values), names(.data))
    cli::cli_abort(c(
      "Can't find variables {.var {missing_names}} in {.arg .data}."
    ))
  }
  if (!.overwrite) {
    values <- values[setdiff(names(values), already)]
  }
  for (v in intersect(names(values), names(.data))) {
    val_labels(.data[[v]], null_action = .null_action) <- values[[v]]
  }

  .data
}

#' @rdname val_labels
#' @export
add_value_labels <- function(
    .data,
    ...,
    .strict = TRUE,
    .null_action = c("unclass", "labelled")) {
  .null_action <- match.arg(.null_action)

  # survey.design case
  if (inherits(.data, "survey.design") || inherits(.data, "svyrep.design")) {
    .data$variables <-
      add_value_labels(
        .data$variables,
        ...,
        .strict = .strict,
        .null_action = .null_action
      )
    return(.data)
  }

  if (!is.data.frame(.data) && !is.atomic(.data))
    cli::cli_abort(
      "{.arg .data} should be a data frame, a survey design or a vector."
    )

  # vector case
  if (is.atomic(.data)) {
    values <- unlist(rlang::dots_list(...))
    if (is.null(names(values)) || any(names(values) == ""))
      cli::cli_abort("All arguments should be named.")
    for (v in names(values)) {
      val_label(.data, values[[v]], null_action = .null_action) <- v
    }
    return(.data)
  }

  # data.frame case
  values <- rlang::dots_list(...)
  if (.strict && !all(names(values) %in% names(.data))) {
    missing_names <- setdiff(names(values), names(.data))
    cli::cli_abort(c(
      "Can't find variables {.var {missing_names}}  in {.arg .data}."
    ))
  }

  for (v in values) {
    if (is.null(names(v)) || any(names(v) == ""))
      cli::cli_abort("All arguments should be named vectors.")
  }

  for (v in intersect(names(values), names(.data))) {
    for (l in names(values[[v]])) {
      val_label(.data[[v]], values[[v]][[l]], null_action = .null_action) <- l
    }
  }

  .data
}

#' @rdname val_labels
#' @export
remove_value_labels <- function(
    .data,
    ...,
    .strict = TRUE,
    .null_action = c("unclass", "labelled")) {
  .null_action <- match.arg(.null_action)

  # survey.design case
  if (inherits(.data, "survey.design") || inherits(.data, "svyrep.design")) {
    .data$variables <-
      remove_value_labels(
        .data$variables,
        ...,
        .strict = .strict,
        .null_action = .null_action
      )
    return(.data)
  }

  if (!is.data.frame(.data) && !is.atomic(.data))
    cli::cli_abort(
      "{.arg .data} should be a data frame, a survey design or a vector."
    )

  # vector case
  if (is.atomic(.data)) {
    values <- unlist(rlang::dots_list(...))
    for (v in values) {
      val_label(.data, v, null_action = .null_action) <- NULL
    }
    return(.data)
  }

  # data.frame case
  values <- rlang::dots_list(...)
  if (.strict && !all(names(values) %in% names(.data))) {
    missing_names <- setdiff(names(values), names(.data))
    cli::cli_abort(c(
      "Can't find variables {.var {missing_names}}  in {.arg .data}."
    ))
  }

  for (v in intersect(names(values), names(.data))) {
    for (l in values[[v]]) {
      val_label(.data[[v]], l, null_action = .null_action) <- NULL
    }
  }

  .data
}

#' Sort value labels
#'
#' Sort value labels according to values or to labels
#'
#' @param x A labelled vector or a data.frame
#' @param according_to According to values or to labels?
#' @param decreasing In decreasing order?
#' @examples
#' v <- labelled(c(1, 2, 3), c(maybe = 2, yes = 1, no = 3))
#' v
#' sort_val_labels(v)
#' sort_val_labels(v, decreasing = TRUE)
#' sort_val_labels(v, "l")
#' sort_val_labels(v, "l", TRUE)
#' @export
sort_val_labels <- function(
    x, according_to = c("values", "labels"),
    decreasing = FALSE) {
  UseMethod("sort_val_labels")
}

#' @export
sort_val_labels.default <- function(
    x,
    according_to = c("values", "labels"),
    decreasing = FALSE) {
  # do nothing
  x
}

#' @export
sort_val_labels.haven_labelled <- function(x,
                                           according_to = c("values", "labels"),
                                           decreasing = FALSE) {
  according_to <- match.arg(according_to)
  labels <- val_labels(x)
  if (!is.null(labels)) {
    if (according_to == "values")
      labels <- sort_tagged_na(labels, decreasing = decreasing)
    if (according_to == "labels")
      labels <- labels[order(names(labels), decreasing = decreasing)]
    val_labels(x) <- labels
  }
  x
}

#' @export
sort_val_labels.data.frame <- function(
    x,
    according_to = c("values", "labels"),
    decreasing = FALSE) {
  x[] <- lapply(x, sort_val_labels,
    according_to = according_to,
    decreasing = decreasing
  )
  x
}

#' @export
sort_val_labels.survey.design <- function(
    x,
    according_to = c("values", "labels"),
    decreasing = FALSE) {
  sort_val_labels(
    x$variables,
    according_to = according_to,
    decreasing = decreasing
  )
}

#' @export
sort_val_labels.svyrep.design <- sort_val_labels.survey.design

#' Turn a named vector into a vector of names prefixed by values
#' @param x vector to be prefixed
#' @param sep (string) separator between value and name
#' @examples
#' df <- dplyr::tibble(
#'   c1 = labelled(c("M", "M", "F"), c(Male = "M", Female = "F")),
#'   c2 = labelled(c(1, 1, 2), c(Yes = 1, No = 2))
#' )
#' val_labels(df$c1)
#' val_labels(df$c1) %>% names_prefixed_by_values()
#' val_labels(df)
#' val_labels(df) %>% names_prefixed_by_values()
#' val_labels(df) %>% names_prefixed_by_values(sep = ":")
#' @export
names_prefixed_by_values <- function(x, sep = "[]") {
  UseMethod("names_prefixed_by_values")
}

#' @export
names_prefixed_by_values.default <- function(x, sep = "[]") {
  if (is.null(x)) {
    return(NULL)
  }
  res <- as.character(x)
  if (is.double(x)) {
    res[is_tagged_na(x)] <- format_tagged_na(x[is_tagged_na(x)])
  }
  if (sep == "[]") {
    res <- paste0("[", res, "] ", names(x))
  } else {
    res <- paste0(res, sep, names(x))
  }
  names(res) <- names(x)
  res
}

#' @export
names_prefixed_by_values.list <- function(x, sep = "[]") {
  lapply(x, names_prefixed_by_values, sep = sep)
}
