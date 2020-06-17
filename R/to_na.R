#' Recode values with no label to NA
#'
#' For labelled variables, values with no label will be recoded to `NA`.
#'
#' @param x Object to recode.
#' @examples
#' v <- labelled(c(1, 2, 9, 1, 9), c(yes = 1, no = 2))
#' nolabel_to_na(v)
#' @export
nolabel_to_na <- function(x) {
  UseMethod("nolabel_to_na")
}

#' @export
nolabel_to_na.default <- function(x) {
  x
}

#' @export
nolabel_to_na.haven_labelled <- function(x) {
  allval <- unique(x)
  allval <- allval[!is.na(allval)]
  nolabel <- allval[!allval %in% val_labels(x)]
  if (length(nolabel) > 0)
    x[x %in% nolabel] <- NA
  x
}

#' @export
nolabel_to_na.data.frame <- function(x) {
  x[] <- lapply(x, nolabel_to_na)
  x
}

#' Recode value labels  to NA
#'
#' For labelled variables, values with a label will be recoded to `NA`.
#'
#' @param x Object to recode.
#' @seealso [haven::zap_labels()]
#' @examples
#' v <- labelled(c(1, 2, 9, 1, 9), c(dk = 9))
#' val_labels_to_na(v)
#' @export
val_labels_to_na <- function(x) {
  UseMethod("val_labels_to_na")
}

#' @export
val_labels_to_na.default <- function(x) {
  x
}

#' @export
val_labels_to_na.haven_labelled <- function(x) {
  val <- val_labels(x)
  if (length(val) > 0)
    x[x %in% val] <- NA
  val_labels(x) <- NULL
  x
}

#' @export
val_labels_to_na.data.frame <- function(x) {
  x[] <- lapply(x, val_labels_to_na)
  x
}


