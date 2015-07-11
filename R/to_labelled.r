#' Convert variable / value attributes to labelled data
#'
#' Convert data imported with \code{\link[foreign]{read.spss}} or \code{\link[foreign]{read.dta}}
#' from \pkg{foreign} package to labelled data, i.e. using \code{\link{labelled}} class.
#'
#' @param x A data set to convert to labelled data frame
#' @details
#' #' Factors will not be converted. Therefore, you should use \code{use.value.labels = FALSE}
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
#' @seealso \code{\link{labelled}}, \code{\link[foreign]{read.spss}}, \code{\link[foreign]{read.dta}}
#' @export
to_labelled <- function(x) {
  foreign_to_labelled(x)
}

#' @rdname to_labelled
#' @export
foreign_to_labelled <- function (x) {
  # note: attr(* , "missings") and attr(*, "variable.labels")
  # are lost when applying as.data.frame (if read.spss(to.data.frame = F))
  variable.labels <- attr(x, "variable.labels") # read.spss
  var.labels <- attr(x, "var.labels") # read.dta
  label.table <- attr(x, "label.table") # read.dta
  missings <- attr(x, "missings") # read.spss

  # if imported with read.spss(to.data.frame=FALSE) it's a list, not a df
  if (!is.data.frame(x)) {
    if (requireNamespace("dplyr"))
      x <- dplyr::as_data_frame(x)
    else
      x <- as.data.frame(x, stringsAsFactors = FALSE)
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
    if (!is.null(attr(x[[var]], "value.labels")))
      val_labels(x[[var]]) <- attr(x[[var]], "value.labels")
    attr(x[[var]], "value.labels") <- NULL
  }

  # value labels (read.dta)
  if (!is.null(label.table))
    val_labels(x) <- label.table

  # missing values (read.spss)
  for (var in names(missings)) {
    if (missings[[var]]$type %in% c("one", "two", "three")) {
      missing_val(x[[var]], force = TRUE) <- missings[[var]]$value
    }
    if (missings[[var]]$type %in% c("range", "range+1")) {
      m <- unique(x[[var]])
      m <- m[m >= missings[[var]]$value[1] & m <= missings[[var]]$value[2]]
      missing_val(x[[var]], force = TRUE) <- m
    }
    if (missings[[var]]$type == "range+1") {
      missing_val(x[[var]], force = TRUE) <- missings[[var]]$value[3]
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
  if (length(class(x)) == 1)
    class(x) <- c("tbl_df", "tbl", "data.frame")

  x
}
