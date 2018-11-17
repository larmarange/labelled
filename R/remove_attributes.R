#' Remove attributes
#'
#' This function removes specified attributes. When applied to a data.frame, it will also
#' remove recursively the specified attributes to each column of the data.frame.
#'
#' @param x an object
#' @param attributes a character vector indicating attributes to remve
#' @export
#' @examples
#' \dontrun{
#' library(haven)
#' path <- system.file("examples", "iris.sav", package = "haven")
#' d <- read_sav(path)
#' str(d)
#' d <- remove_attributes(d, "format.spss")
#' str(d)}
remove_attributes <- function(x, attributes) {
  UseMethod("remove_attributes")
}

#' @export
remove_attributes.default <- function(x, attributes) {
  for (a in attributes) attr(x, a) <- NULL
  x
}

#' @export
remove_attributes.data.frame <- function(x, attributes) {
  cl <- class(x)
  x <- remove_attributes.default(x, attributes)
  x <- dplyr::as_data_frame(
    lapply(x, remove_attributes, attributes = attributes)
  )
  class(x) <- cl
  x
}
