#' Arithmetic mean for numeric labelled vectors
#'
#' @param x a numeric labelled vector.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each end of x .
#' before the mean is computed. Values of trim outside that range are taken as the nearest endpoint.
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation
#' proceeds.
#' @param missing_to_na a logical value indicating whether self-defined missing values should be
#' recoded to \code{NA} before the computation proceeds.
#' @param ... additional arguments to be passed to or from methods.
#'
#' @seealso \code{\link[base]{mean}}
#'
#' @export
mean.labelled <- function(x, trim = 0, na.rm = FALSE, missing_to_na = NULL,
  ...) {
  if (typeof(x) == "character")
    stop("mean could be used only with numeric labelled vectors.")
  if (is.null(missing_to_na)) {
    missing_to_na <- FALSE
    if (!is.null(missing_val(x)))
      warning("x has self-defined missing values and missing_to_na is not specified")
  }

  if (missing_to_na) {
    x <- missing_to_na(x)
  }
  mean.default(x, trim, na.rm)
}
