#' Get / Set value labels
#'
#' @param x A vector.
#' @param prefixed Should labels be prefixed with values?
#' @param v A single value.
#' @param value A named vector for \code{val_labels} (see \code{\link{labelled}}) or a character string
#'   for \code{\link{val_labels}}. \code{NULL} to remove the labels.
#'   For data frames, it could also be a named list.
#' @return
#'   \code{val_labels} will return a named vector.
#'   \code{val_label} will return a single character string.
#' @examples
#' v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9))
#' val_labels(v)
#' val_labels(v, prefixed = TRUE)
#' val_label(v, 2)
#' val_label(v, 2) <- "maybe"
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
val_labels.labelled <- function(x, prefixed = FALSE) {
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
    x <- labelled(x, value)
  }
  x
}

#' @rdname val_labels
#' @export
`val_labels<-.character` <- function(x, value) {
  if (!is.null(value)) {
    x <- labelled(x, value)
  }
  x
}

#' @rdname val_labels
#' @export
`val_labels<-.labelled` <- function(x, value) {
  if (is.null(value)) {
    x <- unclass(x)
    attr(x, "labels") <- NULL
    attr(x, "is_na") <- NULL

  } else {
    if (typeof(value) != typeof(x))
      stop("`x` and `value` must be same type", call. = FALSE, domain = "R-labelled")
    if (is.null(names(value)))
      stop("`value` must be a named vector", call. = FALSE, domain = "R-labelled")
    if (length(value) != length(unique(value)))
      stop("each value should be unique", call. = FALSE, domain = "R-labelled")
    if (any(!missing_val(x) %in% value)) {
      rm_missing_val <- missing_val(x)[!missing_val(x) %in% value]
    }
    attr(x, "is_na") <-
      value %in% missing_val(x) # changing is_na before labels
    attr(x, "labels") <- value
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

  for (var in names(value))
    if (!is.null(value[[var]]))
      if (typeof(x[[var]]) != typeof(value[[var]]))
        stop("`x` and `value` must be same type", call. = FALSE, domain = "R-labelled")

  for (var in names(value))
    val_labels(x[[var]]) <- value[[var]]

  x
}


#' @rdname val_labels
#' @export
val_label <- function(x, v, prefixed = FALSE) {
  UseMethod("val_label")
}

#' @export
val_label.default <- function(x, v, prefixed = FALSE) {
  # return nothing
  NULL
}

#' @rdname val_labels
#' @export
val_label.labelled <- function(x, v, prefixed = FALSE) {
  if (length(v) != 1)
    stop("`v` should be a single value", call. = FALSE, domain = "R-labelled")
  labels <- val_labels(x)
  if (v %in% labels)
    if (prefixed)
      paste0("[", v, "] ", names(labels)[labels == v])
  else
    names(labels)[labels == v]
  else
    NULL
}

#' @rdname val_labels
#' @export
`val_label<-` <- function(x, v, value) {
  UseMethod("val_label<-")
}

#' @rdname val_labels
#' @export
`val_label<-.labelled` <- function(x, v, value) {
  if (length(v) != 1)
    stop("`v` should be a single value", call. = FALSE, domain = "R-labelled")
  if (length(value) > 1)
    stop(
      "`value` should be a single character string or NULL", call. = FALSE, domain = "R-labelled"
    )

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
  `val_label<-.labelled`(x = x, v = v, value = value)
}

#' @rdname val_labels
#' @export
`val_label<-.character` <- function(x, v, value) {
  `val_label<-.labelled`(x = x, v = v, value = value)
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
      stop(
        "`value` should contain only characters or NULL",
        call. = FALSE, domain = "R-labelled"
      )
    if (length(value[[var]]) > 1)
      stop(
        "`value` should contain only one string (or NULL) per variable",
        call. = FALSE, domain = "R-labelled"
      )
  }

  for (var in names(value))
    val_label(x[[var]], v) <- value[[var]]

  x
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
#' sort_val_labels(v, decreasing = T)
#' sort_val_labels(v, "l")
#' sort_val_labels(v, "l", T)
#' @export
sort_val_labels <-
  function(x, according_to = c("values", "labels"), decreasing = FALSE) {
    UseMethod("sort_val_labels")
  }

#' @export
sort_val_labels.default <-
  function(x, according_to = c("values", "labels"), decreasing = FALSE) {
    # do nothing
    x
  }

#' @rdname sort_val_labels
#' @export
sort_val_labels.labelled <-
  function(x, according_to = c("values", "labels"), decreasing = FALSE) {
    according_to <- match.arg(according_to)
    labels <- val_labels(x)
    if (!is.null(labels)) {
      if (according_to == "values")
        labels <- sort(labels, decreasing = decreasing)
      else if (according_to == "labels")
        labels <-
          labels[order(names(labels), decreasing = decreasing)]
      val_labels(x) <- labels
    }
    x
  }

#' @rdname sort_val_labels
#' @export
sort_val_labels.data.frame <-
  function(x, according_to = c("values", "labels"), decreasing = FALSE) {
    x[] <- lapply(x, sort_val_labels, according_to = according_to, decreasing = decreasing)
    x
  }
