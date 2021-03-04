#' Recode some values based on condition
#'
#' @param x vector to be recoded
#' @param condition logical vector of same length as `x`
#' @param true values to use for `TRUE` values of `condition`. It must be
#' either the same length as `x`, or length 1.
#' @return Returns `x` with values replaced by `true` when `condition` is
#' `TRUE` and unchanged when `condition` is `FALSE` or `NA`. Variable and value
#' labels are preserved unchanged.
#' @export
#' @examples
#' v <- labelled(c(1,2,2,9), c(yes = 1, no = 2))
#' v %>% recode_if(v == 9, NA)
#' if (require(dplyr)) {
#'   df <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 2, 1)) %>%
#'     set_value_labels(s1 = c(Male = "M", Female = "F"), s2 = c(A = 1, B = 2)) %>%
#'     set_variable_labels(s1 = "Gender", s2 = "Group")
#'
#'   df <- df %>%
#'     mutate(
#'       s3 = s2 %>% recode_if(s1 == "F", 2),
#'       s4 = s2 %>% recode_if(s1 == "M", s2 + 10)
#'     )
#'   df
#'   df %>% look_for()
#' }
recode_if <- function(x, condition, true) {
  if (!is.logical(condition))
    stop("'condition' should be logical.")
  if (length(x) != length(condition))
    stop("'condition' and 'x' should have the same length.")
  if (length(true) > 1 & length(true) != length(x))
    stop("'true' should be unique or of same length as 'x'.")

  original_class <- class(x)

  condition[is.na(condition)] <- FALSE

  if (length(true) == 1) {
    x[condition] <- true
  } else {
    x[condition] <- true[condition]
  }

  if (!identical(class(x), original_class))
    warning(
      "Class of 'x' has changed and is now equal to \"",
      paste(class(x), collapse = ", "),
      "\".\n",
      "This is usually the case when class of 'value' is different from `x`\n.",
      "and forced R to coerce 'x' to the class of 'value'."
    )

  x
}
