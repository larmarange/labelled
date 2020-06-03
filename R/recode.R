#' Recode values
#'
#' Extend \code{\link[dplyr]{recode}} method from \pkg{haven} to
#' works with labelled vectors.
#'
#' @importFrom dplyr recode
#' @inheritParams dplyr::recode
#' @param .keep_value_labels If TRUE, keep original value labels. If FALSE, remove value labels.
#' @seealso \code{\link[dplyr]{recode}} (\pkg{dplyr})
#' @examples
#' x <- labelled(1:3, c(yes = 1, no = 2))
#' x
#' dplyr::recode(x, `3` = 2L)
#'
#' # do not keep value labels
#' dplyr::recode(x, `3` = 2L, .keep_value_labels = FALSE)
#'
#' # be careful, changes are not of the same type (here integers), NA are created
#' dplyr::recode(x, `3` = 2)
#'
#' # except if you provide .default or new values for all old values
#' dplyr::recode(x, `1` = 1, `2` = 1,`3` = 2)
#'
#' # if you change the type of the vector (here transformed into character)
#' # value labels are lost
#' dplyr::recode(x, `3` = "b", .default = "a")
#'
#' # use .keep_value_labels = FALSE to avoid a warning
#' dplyr::recode(x, `3` = "b", .default = "a", .keep_value_labels = FALSE)
#' @export
recode.haven_labelled <- function(.x, ..., .default = NULL, .missing = NULL, .keep_value_labels = TRUE) {
  ret <- dplyr::recode(.x = unclass(.x), ..., .default = .default, .missing = .missing)
  if (mode(.x) == mode(ret) & .keep_value_labels) {
    ret <- copy_labels(.x, ret)
  } else {
    var_label(ret) <- var_label(.x)
    if (.keep_value_labels)
      warning("The type of .x has been changed and value labels attributes have been lost.")
  }
  ret
}
