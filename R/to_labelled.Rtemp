#' Convert to labelled data
#'
#' Convert data imported with \pkg{foreign} or \pkg{memisc} to labelled data.
#' \code{to_labelled} could also be used to convert a factor to a labelled vector.
#'
#' @param x Data to convert to labelled data frame
#' @details
#' \code{to_labelled} is a general wrapper calling the appropriate sub-functions.
#'
#' \code{memisc_to_labelled} converts a \code{\link[memisc]{data.set}} object created with
#' \pkg{memisc} package to a labelled data frame.
#'
#' \code{foreign_to_labelled} converts data imported with \code{\link[foreign]{read.spss}}
#' or \code{\link[foreign]{read.dta}} from \pkg{foreign} package to a labelled data frame,
#' i.e. using \code{\link{labelled}} class.
#'
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
#' @return a tbl data frame or a labelled vector.
#' @seealso \code{\link{labelled}} (\pkg{foreign}), \code{\link[foreign]{read.spss}} (\pkg{foreign}),
#'   \code{\link[foreign]{read.dta}} (\pkg{foreign}), \code{\link[memisc]{data.set}} (\pkg{memisc}),
#'   \code{\link[memisc]{importer}} (\pkg{memisc}).
#'
#' @examples
#' \dontrun{
#'   # from foreign
#'   library(foreign)
#'   df <- to_labelled(read.spss(
#'     'file.sav',
#'     to.data.frame = FALSE,
#'     use.value.labels = FALSE,
#'     use.missings = FALSE
#'  ))
#'  df <- to_labelled(read.dta(
#'    'file.dta',
#'    convert.factors = FALSE
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
to_labelled <- function(x) {
  UseMethod("to_labelled")
}

#' @rdname to_labelled
#' @export
to_labelled.data.frame <- function(x) {
  foreign_to_labelled(x)
}

#' @rdname to_labelled
#' @export
to_labelled.list <- function(x) {
  foreign_to_labelled(x)
}

#' @rdname to_labelled
#' @export
to_labelled.data.set <- function(x) {
  memisc_to_labelled(x)
}

#' @rdname to_labelled
#' @export
to_labelled.importer <- function(x) {
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
  #missings <- attr(x, "missings", exact = TRUE)  # read.spss

  # if imported with read.spss(to.data.frame=FALSE) it's a
  # list, not a df
  if (!is.data.frame(x)) {
    if (requireNamespace("dplyr")) {
      x <- dplyr::as_data_frame(x)
    } else {
      x <- as.data.frame(x, stringsAsFactors = FALSE)
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
    .setattr(x[[var]], "value.labels", NULL)
  }

  # value labels (read.dta)
  if (!is.null(label.table))
    val_labels(x) <- label.table

  # missing values (read.spss)
  # for (var in names(missings)) {
  #   if (missings[[var]]$type %in% c("one", "two", "three")) {
  #     missing_val(x[[var]], force = TRUE) <- missings[[var]]$value
  #   }
  #   if (missings[[var]]$type %in% c("range", "range+1")) {
  #     m <- unique(x[[var]])
  #     m <- m[m >= missings[[var]]$value[1] & m <= missings[[var]]$value[2]]
  #     missing_val(x[[var]], force = TRUE) <- m
  #   }
  #   if (missings[[var]]$type == "range+1") {
  #     missing_val(x[[var]], force = TRUE) <- missings[[var]]$value[3]
  #   }
  # }

  # cleaning read.spss
  .setattr(x, "variable.labels", NULL)
  .setattr(x, "missings", NULL)
  # cleaning read.dta
  .setattr(x, "datalabel", NULL)
  .setattr(x, "time.stamp", NULL)
  .setattr(x, "formats", NULL)
  .setattr(x, "types", NULL)
  .setattr(x, "val.labels", NULL)
  .setattr(x, "var.labels", NULL)
  .setattr(x, "version", NULL)
  .setattr(x, "label.table", NULL)
  .setattr(x, "missing", NULL)
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
#' @export
to_labelled.factor <- function(x) {
  labs <- 1:length(levels(x))
  names(labs) <- levels(x)
  x <- labelled(as.numeric(x), labs)
  x
}
