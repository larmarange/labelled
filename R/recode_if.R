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
#' v <- labelled(c(1, 2, 2, 9), c(yes = 1, no = 2))
#' v %>% recode_if(v == 9, NA)
#' if (require(dplyr)) {
#'   df <- tibble(s1 = c("M", "M", "F"), s2 = c(1, 2, 1)) %>%
#'     set_value_labels(
#'       s1 = c(Male = "M", Female = "F"),
#'       s2 = c(A = 1, B = 2)
#'     ) %>%
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
  check_logical(condition)
  if (length(x) != length(condition))
    cli::cli_abort(paste(
      "{.arg condition} (length: {length(condition)}) and",
      "{.arg x} (length: {length(x)}) should have the same length."
    ))
  if (length(true) > 1 && length(true) != length(x))
    cli::cli_abort(
      "{.arg true} should be unique or of same length as {.arg x}."
    )

  original_class <- class(x)

  condition[is.na(condition)] <- FALSE

  if (length(true) == 1) {
    x[condition] <- true
  } else {
    x[condition] <- true[condition]
  }

  if (!identical(class(x), original_class))
    cli::cli_warn(paste(
      "Class of {.arg x} (originally {.field {original_class}}) has changed",
      "and was coerced to {.field {class(x)}}."
    ))

  x
}
