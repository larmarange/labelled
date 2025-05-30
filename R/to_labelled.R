#' Convert to labelled data
#'
#' Convert a factor or data imported with \pkg{foreign} or \pkg{memisc} to
#' labelled data.
#'
#' @param x Factor or dataset to convert to labelled data frame
#' @param ... Not used
#' @details
#' `to_labelled()` is a general wrapper calling the appropriate sub-functions.
#'
#' `memisc_to_labelled()` converts a `memisc::data.set()`]` object created with
#' \pkg{memisc} package to a labelled data frame.
#'
#' `foreign_to_labelled()` converts data imported with [foreign::read.spss()]
#' or [foreign::read.dta()] from \pkg{foreign} package to a labelled data frame,
#' i.e. using [haven::labelled()].
#' Factors will not be converted. Therefore, you should use
#' `use.value.labels = FALSE` when importing with [foreign::read.spss()] or
#' `convert.factors = FALSE` when importing with [foreign::read.dta()].
#'
#' To convert correctly defined missing values imported with
#' [foreign::read.spss()], you should have used `to.data.frame = FALSE` and
#' `use.missings = FALSE`. If you used the option `to.data.frame = TRUE`,
#' meta data describing missing values will not be attached to the import.
#' If you used `use.missings = TRUE`, missing values would have been converted
#' to `NA`.
#'
#' So far, missing values defined in **Stata** are always imported as `NA` by
#' [foreign::read.dta()] and could not be retrieved by `foreign_to_labelled()`.
#'
#' @return A tbl data frame or a labelled vector.
#' @seealso [haven::labelled()], [foreign::read.spss()],
#'   [foreign::read.dta()], `memisc::data.set()`,
#'   `memisc::importer`, [to_factor()].
#'
#' @examples
#' \dontrun{
#' # from foreign
#' library(foreign)
#' sav <- system.file("files", "electric.sav", package = "foreign")
#' df <- to_labelled(read.spss(
#'   sav,
#'   to.data.frame = FALSE,
#'   use.value.labels = FALSE,
#'   use.missings = FALSE
#' ))
#'
#' # from memisc
#' library(memisc)
#' nes1948.por <- UnZip("anes/NES1948.ZIP", "NES1948.POR", package = "memisc")
#' nes1948 <- spss.portable.file(nes1948.por)
#' ds <- as.data.set(nes1948)
#' df <- to_labelled(ds)
#' }
#'
#' @export
to_labelled <- function(x, ...) {
  UseMethod("to_labelled")
}

#' @rdname to_labelled
#' @export
to_labelled.data.frame <- function(x, ...) {
  foreign_to_labelled(x)
}

#' @rdname to_labelled
#' @export
to_labelled.list <- function(x, ...) {
  foreign_to_labelled(x)
}

#' @rdname to_labelled
#' @export
to_labelled.data.set <- function(x, ...) {
  memisc_to_labelled(x)
}

#' @rdname to_labelled
#' @export
to_labelled.importer <- function(x, ...) {
  memisc_to_labelled(memisc::as.data.set(x))
}


#' @rdname to_labelled
#' @export
foreign_to_labelled <- function(x) {
  # note: attr(* , 'missings') and attr(*, 'variable.labels')
  # are lost when applying as.data.frame (if
  # read.spss(to.data.frame = F))
  variable.labels <- attr(x, "variable.labels", exact = TRUE) # read.spss
  var.labels <- attr(x, "var.labels", exact = TRUE) # read.dta
  label.table <- attr(x, "label.table", exact = TRUE) # read.dta
  missings <- attr(x, "missings", exact = TRUE) # read.spss

  # if imported with read.spss(to.data.frame=FALSE) it's a
  # list, not a df
  if (!is.data.frame(x)) {
    x <- dplyr::as_tibble(x)
  }

  # variable labels (read.spss)
  if (!is.null(variable.labels)) {
    var_label(x) <- as.list(variable.labels)
  }

  # variable labels (read.dta)
  if (!is.null(var.labels)) {
    names(var.labels) <- names(x)
    var_label(x) <- as.list(var.labels)
  }

  # value labels (read.spss)
  for (var in names(x)) {
    if (!is.null(attr(x[[var]], "value.labels", exact = TRUE))) {
      val_labels(x[[var]]) <- attr(x[[var]], "value.labels",
        exact = TRUE
      )
    }
    attr(x[[var]], "value.labels") <- NULL
  }

  # value labels (read.dta)
  if (!is.null(label.table)) {
    # taking into account only variables existing in x
    val_labels(x) <- label.table[intersect(names(label.table), names(x))]
  }


  # missing values (read.spss)
  for (var in names(missings)) {
    if (missings[[var]]$type %in% c("one", "two", "three")) {
      na_values(x[[var]]) <- missings[[var]]$value
    }
    if (missings[[var]]$type %in% c("range", "range+1")) {
      na_range(x[[var]]) <- missings[[var]]$value[1:2]
    }
    if (missings[[var]]$type == "range+1") {
      na_values(x[[var]]) <- missings[[var]]$value[3]
    }
  }

  # cleaning read.spss
  attr(x, "variable.labels") <- NULL
  attr(x, "missings") <- NULL
  # cleaning read.dta
  attr(x, "datalabel") <- NULL
  attr(x, "time.stamp") <- NULL
  attr(x, "formats") <- NULL
  attr(x, "types") <- NULL
  attr(x, "val.labels") <- NULL
  attr(x, "var.labels") <- NULL
  attr(x, "version") <- NULL
  attr(x, "label.table") <- NULL
  attr(x, "missing") <- NULL
  # to tbl_df (if no other class already specified)
  if (length(class(x)) == 1) {
    class(x) <- c("tbl_df", "tbl", "data.frame")
  }

  x
}

#' @rdname to_labelled
#' @export
memisc_to_labelled <- function(x) {
  if (!inherits(x, "data.set")) {
    return(x)
  }

  rlang::check_installed("memisc", "to convert a data.set.")

  df <- as.data.frame(x)
  for (var in names(x)) {
    if (length(memisc::description(x[[var]])) > 0) {
      var_label(df[[var]]) <- as.character(memisc::description(x[[var]]))
    }
    if (length(memisc::labels(x[[var]])) > 0) {
      labs <- memisc::labels(x[[var]])@values
      names(labs) <- memisc::labels(x[[var]])@.Data
      val_labels(df[[var]]) <- labs
    }
    if (
      !is.null(memisc::missing.values(x[[var]])) &&
        length(memisc::missing.values(x[[var]])@filter) > 0
    ) {
      na_values(df[[var]]) <- memisc::missing.values(x[[var]])@filter
    }
    if (
      !is.null(memisc::missing.values(x[[var]])) &&
        length(memisc::missing.values(x[[var]])@range) > 0
    ) {
      na_range(df[[var]]) <- memisc::missing.values(x[[var]])@range
    }
  }

  dplyr::as_tibble(df)
}

#' @rdname to_labelled
#' @param labels When converting a factor only:
#'   an optional named vector indicating how factor levels should be coded.
#'   If a factor level is not found in `labels`, it will be converted to `NA`.
#' @param .quiet do not display warnings for prefixed factors with duplicated
#'   codes
#' @details
#' If you convert a labelled vector into a factor with prefix, i.e. by using
#' [`to_factor(levels = "prefixed")`][to_factor()], `to_labelled.factor()` is
#' able to reconvert it to a labelled vector with same values and labels.
#' @export
#' @examples
#' # Converting factors to labelled vectors
#' f <- factor(
#'   c("yes", "yes", "no", "no", "don't know", "no", "yes", "don't know")
#' )
#' to_labelled(f)
#' to_labelled(f, c("yes" = 1, "no" = 2, "don't know" = 9))
#' to_labelled(f, c("yes" = 1, "no" = 2))
#' to_labelled(f, c("yes" = "Y", "no" = "N", "don't know" = "DK"))
#'
#' s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
#' labels <- val_labels(s1)
#' f1 <- to_factor(s1)
#' f1
#'
#' to_labelled(f1)
#' identical(s1, to_labelled(f1))
#' to_labelled(f1, labels)
#' identical(s1, to_labelled(f1, labels))
#'
#' l <- labelled(
#'   c(1, 1, 2, 2, 9, 2, 1, 9),
#'   c("yes" = 1, "no" = 2, "don't know" = 9)
#' )
#' f <- to_factor(l, levels = "p")
#' f
#' to_labelled(f)
#' identical(to_labelled(f), l)
to_labelled.factor <- function(x, labels = NULL, .quiet = FALSE, ...) {
  vl <- var_label(x)
  if (is.null(labels)) {
    # check if levels are formatted as "[code] label"
    l <- .get_prefixes.factor(x)
    if (anyNA(l$code) || anyNA(l$code) || any(duplicated(l$code))) {
      if (
        !.quiet &&
          any(duplicated(l$code)) &&
          !anyNA(l$code) &&
          !anyNA(l$code)
      ) {
        cli::cli_warn("{.arg x} looks prefixed, but duplicated codes found.")
      }
      # normal case
      labs <- seq_along(levels(x))
      names(labs) <- levels(x)
      x <- labelled(as.numeric(x), labs)
    } else {
      # "[code] label" case
      num_l <- suppressWarnings(as.numeric(l$code))
      if (!.quiet && !anyNA(num_l) && any(duplicated(num_l))) {
        cli::cli_warn("All codes seem numeric but some duplicates found.")
      }
      if (!anyNA(num_l) && !any(duplicated(num_l))) {
        l$code <- as.numeric(l$code)
      }
      r <- l$levels
      names(r) <- l$code
      levels(x) <- l$code
      x <- as.character(x)
      if (is.numeric(l$code)) {
        x <- as.numeric(x)
      }
      names(l$code) <- l$label
      x <- labelled(x, l$code)
    }
  } else {
    # labels is not NULL
    r <- rep_len(NA, length(x))
    mode(r) <- mode(labels)
    for (i in seq_along(labels)) {
      r[x == names(labels)[i]] <- labels[i]
    }
    x <- labelled(r, labels)
  }
  var_label(x) <- vl
  x
}
