#' Convert factor to labelled
#'
#' Convert a factor to a numerical
#'
#' @param x factor to convert
#'
#' @note
#' Note that due to the way that \R store internally factors,
#' \code{as_labelled(as_factor(x))} will usually not equal
#' to \code{x} (see examples).
#'
#' @examples
#' x <- factor(c("a", "a", "c", "b", "b"))
#' as_labelled(x)
#'
#' y <- labelled(c(2, 1, 9, 1, 2), c(yes = 1, no = 2, dk = 9))
#' y
#' as_labelled(as_factor(y))
#' @export
as_labelled <- function(x) {
  UseMethod("as_labelled")
}

#' @rdname as_labelled
#' @export
as_labelled.factor <- function(x) {
  labs <- 1:length(levels(x))
  names(labs) <- levels(x)
  x <- labelled(as.numeric(x), labs)
  x
}
