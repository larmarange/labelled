#' Recode values
#'
#' Extend \code{\link[dplyr]{recode}} method from \pkg{haven} to
#' works with labelled vectors.
#'
#' @importFrom dplyr recode
#' @inheritParams dplyr::recode
#' @seealso \code{\link[dplyr]{recode}} (\pkg{dplyr})
#' @examples
#' x <- labelled(1:3, c(yes = 1, no = 2))
#' x
#' dplyr::recode(x, `3` = 2L)
#' @export
recode.haven_labelled <- function(.x, ..., .default = NULL, .missing = NULL) {
  ret <- dplyr::recode(.x = unclass(.x), ..., .default = .default, .missing = .missing)
  if (mode(.x) == mode(ret)) {
    ret <- copy_labels(.x, ret)
  } else {
    var_label(ret) <- var_label(.x)
    warning("The type of .x has been changed and value labels attributes have been lost.")
  }
  ret
}
