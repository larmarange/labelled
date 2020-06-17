#' Recode values
#'
#' Extend [dplyr::recode()] method from \pkg{dplyr} to
#' works with labelled vectors.
#'
#' @importFrom dplyr recode
#' @inheritParams dplyr::recode
#' @param .keep_value_labels If TRUE, keep original value labels.
#'   If FALSE, remove value labels.
#' @param .combine_value_labels If TRUE, will combine original value labels
#'   to generate new value labels. Note that unexpected results could be obtained
#'   if a same old value is recoded into several different new values.
#' @param .sep Separator to be used when combining value labels.
#' @seealso [dplyr::recode()]
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
#'
#' # combine value labels
#' x <- labelled(1:4, c("strongly agree" = 1, "agree" = 2, "disagree" = 3, "strongly disagree" = 4))
#' dplyr::recode(x, `1` = 1L, `2` = 1L, `3` = 2L, `4` = 2L, .combine_value_labels = TRUE)
#' dplyr::recode(x, `2` = 1L, `4` = 3L, .combine_value_labels = TRUE)
#' dplyr::recode(x, `2` = 1L, `4` = 3L, .combine_value_labels = TRUE, .sep = " or ")
#' dplyr::recode(x, `2` = 1L, .default = 2L, .combine_value_labels = TRUE)
#'
#' # example when combining some values without a label
#' y <- labelled(1:4, c("strongly agree" = 1))
#' dplyr::recode(y, `2` = 1L, `4` = 3L, .combine_value_labels = TRUE)
#' @export
recode.haven_labelled <- function(
  .x,
  ...,
  .default = NULL,
  .missing = NULL,
  .keep_value_labels = TRUE,
  .combine_value_labels = FALSE,
  .sep = " / "
) {
  ret <- dplyr::recode(.x = unclass(.x), ..., .default = .default, .missing = .missing)
  if (mode(.x) == mode(ret)) {
    if (.keep_value_labels) {
      ret <- copy_labels(.x, ret)
    }
    if (.combine_value_labels) {
      ret <- copy_labels(.x, ret)

      old_vals <- unique(.x)
      new_vals <- c()
      for (o in old_vals) {
        new_vals <- c(new_vals, ret[.x == o][1])
      }

      original_labels <- val_labels(.x)
      for (v in unique(new_vals)) {
        combined_label <- names(original_labels[original_labels %in% old_vals[new_vals == v]])
        if (length(combined_label) > 0)
          val_label(ret, v) <- paste(combined_label, collapse = .sep)
      }
      ret <- drop_unused_value_labels(ret)
    }
  } else {
    var_label(ret) <- var_label(.x)
    if (.keep_value_labels | .combine_value_labels)
      warning("The type of .x has been changed and value labels attributes have been lost.")
  }
  ret
}
