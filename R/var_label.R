#' Get / Set a variable label
#'
#' @param x A vector.
#' @param value A character string or \code{NULL} to remove the label.
#' @examples
#' var_label(iris$Sepal.Length)
#' var_label(iris$Sepal.Length) <- "Length of the sepal"
#' \dontrun{
#'  View(iris)
#' }
#' # To remove a variable label
#' var_label(iris$Sepal.Length) <- NULL
#' @export
var_label <- function(x) {
  if (!is.atomic(x))
    stop("`x` should be atomic", call. = FALSE, domain = "R-labelled")
  attr(x, "label", exact = TRUE)
}

#' @rdname var_label
#' @export
`var_label<-` <- function(x, value) {
  if (!is.atomic(x))
    stop("`x` should be atomic", call. = FALSE, domain = "R-labelled")
  if ((!is.character(value) & !is.null(value)) | length(value) > 1)
    stop("`value` should be a single character string or NULL", call. = FALSE, domain = "R-labelled")
  attr(x, "label") <- value
  x
}
