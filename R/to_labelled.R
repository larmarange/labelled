#' Convert to labelled data
#'
#' Convert a factor or data imported with \pkg{foreign} or \pkg{memisc} to labelled data.
#'
#' @param x Factor or dataset to convert to labelled data frame
#' @param ... Not used
#' @details
#' \code{to_labelled} is a general wrapper calling the appropriate sub-functions.
#'
#' \code{memisc_to_labelled} converts a \code{\link[memisc]{data.set}} object created with
#' \pkg{memisc} package to a labelled data frame.
#'
#' \code{foreign_to_labelled} converts data imported with \code{\link[foreign]{read.spss}}
#' or \code{\link[foreign]{read.dta}} from \pkg{foreign} package to a labelled data frame,
#' i.e. using \code{\link{labelled}} class.
#' Factors will not be converted. Therefore, you should use \code{use.value.labels = FALSE}
#' when importing with \code{\link[foreign]{read.spss}} or \code{convert.factors = FALSE} when
#' importing with \code{\link[foreign]{read.dta}}.
#'
#' To convert correctly defined missing values imported with \code{\link[foreign]{read.spss}}, you should
#' have used \code{to.data.frame = FALSE} and \code{use.missings = FALSE}. If you used the option
#' \code{to.data.frame = TRUE}, meta data describing missing values will not be attached to the import.
#' If you used \code{use.missings = TRUE}, missing values would have been converted to \code{NA}.
#'
#' So far, missing values defined in Stata are always imported as \code{NA} by
#' \code{\link[foreign]{read.dta}} and could not be retrieved by \code{foreign_to_labelled}.
#'
#' @return A tbl data frame or a labelled vector.
#' @seealso \code{\link{labelled}} (\pkg{foreign}), \code{\link[foreign]{read.spss}} (\pkg{foreign}),
#'   \code{\link[foreign]{read.dta}} (\pkg{foreign}), \code{\link[memisc]{data.set}} (\pkg{memisc}),
#'   \code{\link[memisc]{importer}} (\pkg{memisc}), \code{\link{to_factor}}.
#'
#' @examples
#' \dontrun{
#'   # from foreign
#'   library(foreign)
#'   sav <- system.file("files", "electric.sav", package = "foreign")
#'   df <- to_labelled(read.spss(
#'     sav,
#'     to.data.frame = FALSE,
#'     use.value.labels = FALSE,
#'     use.missings = FALSE
#'  ))
#'
#'  # from memisc
#'  library(memisc)
#'  nes1948.por <- UnZip('anes/NES1948.ZIP', 'NES1948.POR', package='memisc')
#'  nes1948 <- spss.portable.file(nes1948.por)
#'  df <- to_labelled(nes1948)
#'  ds <- as.data.set(nes19480)
#'  df <- to_labelled(ds)
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
  variable.labels <- attr(x, "variable.labels", exact = TRUE)  # read.spss
  var.labels <- attr(x, "var.labels", exact = TRUE)  # read.dta
  label.table <- attr(x, "label.table", exact = TRUE)  # read.dta
  missings <- attr(x, "missings", exact = TRUE)  # read.spss

  # if imported with read.spss(to.data.frame=FALSE) it's a
  # list, not a df
  if (!is.data.frame(x)) {
    if (requireNamespace("dplyr")) {
      x <- dplyr::as_data_frame(x)
    } else {
      x <- dplyr::as_data_frame(x, stringsAsFactors = FALSE)
    }
  }

  # variable labels (read.spss)
  if (!is.null(variable.labels))
    var_label(x) <- as.list(variable.labels)

  # variable labels (read.dta)
  if (!is.null(var.labels)) {
    names(var.labels) <- names(x)
    var_label(x) <- as.list(var.labels)
  }

  # value labels (read.spss)
  for (var in names(x)) {
    if (!is.null(attr(x[[var]], "value.labels", exact = TRUE)))
      val_labels(x[[var]]) <- attr(x[[var]], "value.labels",
        exact = TRUE)
    attr(x[[var]], "value.labels") <-  NULL
  }

  # value labels (read.dta)
  if (!is.null(label.table))
    val_labels(x) <- label.table

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
  attr(x, "missings") <-  NULL
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
  if (length(class(x)) == 1)
    class(x) <- c("tbl_df", "tbl", "data.frame")

  x
}

#' @rdname to_labelled
#' @export
memisc_to_labelled <- function(x) {
  if (!inherits(x, "data.set"))
    return(x)
  if (!requireNamespace("memisc"))
    stop("memisc package is required to convert a data.set",
      call. = FALSE, domain = "R-labelled")

  df <- as.data.frame(x)
  for (var in names(x)) {
    df[[var]] <- x[[var]]@.Data
    var_label(df[[var]]) <- as.character(memisc::annotation(x[[var]]))
    if (!is.null(memisc::labels(x[[var]]))) {
      labs <- memisc::labels(x[[var]])@values
      names(labs) <- memisc::labels(x[[var]])@.Data
      val_labels(df[[var]]) <- labs
    }
    #missing_val(df[[var]]) <- unique(df[memisc::is.missing(x[[var]]), var])
  }

  unloadNamespace("memisc")

  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}

#' @rdname to_labelled
#' @param labels When converting a factor only:
#'   an optional named vector indicating how factor levels should be coded.
#'   If a factor level is not found in \code{labels}, it will be converted to \code{NA}.
#' @export
#' @examples
#' # Converting factors to labelled vectors
#' f <- factor(c("yes", "yes", "no", "no", "don't know", "no", "yes", "don't know"))
#' to_labelled(f)
#' to_labelled(f, c("yes" = 1, "no" = 2, "don't know" = 9))
#' to_labelled(f, c("yes" = 1, "no" = 2))
#' to_labelled(f, c("yes" = "Y", "no" = "N", "don't know" = "DK"))
#'
#' s1 <- labelled(c('M', 'M', 'F'), c(Male = 'M', Female = 'F'))
#' labels <- val_labels(s1)
#' f1 <- to_factor(s1)
#' f1
#'
#' to_labelled(f1)
#' identical(s1, to_labelled(f1))
#' to_labelled(f1, labels)
#' identical(s1, to_labelled(f1, labels))
to_labelled.factor <- function(x, labels = NULL, ...) {
  if (is.null(labels)) {
    vl <- var_label(x)
    labs <- 1:length(levels(x))
    names(labs) <- levels(x)
    x <- labelled(as.numeric(x), labs)
    var_label(x) <- vl
  } else {
    vl <- var_label(x)
    r <- rep_len(NA, length(x))
    mode(r) <- mode(labels)
    for (i in 1:length(labels))
      r[x == names(labels)[i]] <- labels[i]
    x <- labelled(r, labels)
    var_label(x) <- vl
  }
  x
}
