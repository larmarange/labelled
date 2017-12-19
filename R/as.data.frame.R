#' as.data.frame method for labelled vectors
#'
#' @param x a labelled vector
#' @param ... additional arguments to be passed to or from methods.
#' @export
as.data.frame.labelled <- function(x, ...) {
  if (typeof(x) == "character") {
    as.data.frame.character(x, stringsAsFactors = FALSE, ...)
  } else {
    as.data.frame.numeric(x, ...)
  }
}
