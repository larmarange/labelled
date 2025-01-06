#' Check if a factor is prefixed
#' @param x a factor
#' @export
is_prefixed <- function(x) {
  if (!is.factor(x))
    cli::cli_abort(paste(
      "{.fn is_prefixed} should be used only with a factor",
      "({.arg x} is {class(x)})."
    ))
  l <- .get_prefixes.factor(x)
  !anyNA(l$code) && !anyNA(l$code) && !any(duplicated(l$code))
}


# return a tibble with levels, code and label
# if the factor is prefixed, otherwise NA
.get_prefixes.factor <- function(x) {
  dplyr::tibble(levels = levels(x)) %>%
    tidyr::extract(
      "levels",
      c("code", "label"),
      "^\\[(.+)\\]\\s(.+)$",
      remove = FALSE
    )
}
