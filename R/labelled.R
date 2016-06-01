#' Create a labelled vector.
#'
#' A labelled vector is a common data structure in other statistical
#' environments. This class makes it possible to import such labelled vectors
#' in to without loss of fidelity.
#'
#' @param x A vector to label. Must be either numeric (integer or double) or
#'   character.
#' @param labels A named vector. The vector should be the same type as
#'   x. Unlike factors, labels don't need to be exhaustive: only a fraction
#'   of the values might be labelled.
#' @export
#' @examples
#' s1 <- labelled(c('M', 'M', 'F'), c(Male = 'M', Female = 'F'))
#' s2 <- labelled(c(1, 1, 2), c(Male = 1, Female = 2))
labelled <- function(x, labels) {
  if (!is.numeric(x) && !is.character(x)) {
    stop("`x` must be either numeric or a character vector",
      call. = FALSE, domain = "R-labelled")
  }
  if (mode(x) != mode(labels)) {
    stop("`x` and `labels` must be same type", call. = FALSE,
      domain = "R-labelled")
  }
  if (typeof(x) != typeof(labels)) {
    mode(labels) <- typeof(x)
  }
  if (is.null(names(labels))) {
    stop("`labels` must be a named vector", call. = FALSE,
      domain = "R-labelled")
  }
  if (length(labels) != length(unique(labels))) {
    stop("`each value in `labels` should be unique", call. = FALSE,
      domain = "R-labelled")
  }

  structure(x, labels = labels, class = c("labelled"))
}

#' @rdname labelled
#' @examples
#' is.labelled(s1)
#' is.labelled(c('M', 'M', 'F'))
#' @export
is.labelled <- function(x) inherits(x, "labelled")

#' @export
`[.labelled` <- function(x, ...) {
  labelled(NextMethod(), attr(x, "labels"))
}

#' @export
print.labelled <- function(x, ...) {
  cat("<Labelled ", typeof(x), "> ", var_label(x), "\n", sep = "")


  xx <- unclass(x)
  .setattr(xx, "label", NULL)
  .setattr(xx, "labels", NULL)
  print(xx)

  cat("\nLabels:\n")
  labels <- attr(x, "labels", exact = TRUE)
  lab_df <- data.frame(value = unname(labels), label = names(labels))
  print(lab_df, row.names = FALSE)

  invisible()
}

#' @export
as.data.frame.labelled <- function(x, ...) {
  df <- list(x)
  names(df) <- deparse(substitute(x))
  class(df) <- "data.frame"
  .setattr(df, "row.names", .set_row_names(length(x)))

  df
}


