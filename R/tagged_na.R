#' @importFrom haven tagged_na
#' @export
haven::tagged_na

#' @importFrom haven na_tag
#' @export
haven::na_tag

#' @importFrom haven is_tagged_na
#' @export
haven::is_tagged_na

#' @importFrom haven format_tagged_na
#' @export
haven::format_tagged_na

#' @importFrom haven print_tagged_na
#' @export
haven::print_tagged_na

#' @export
duplicated_tagged_na <- function(x, fromLast = FALSE) {
  if (!is.double(x))
    return(duplicated(x, fromLast = fromLast))

  res <- duplicated(x, fromLast = fromLast, incomparables = NA)

  if (anyNA(x))
    res[is.na(x)] <- duplicated(
      format_tagged_na(x[is.na(x)]),
      fromLast = fromLast
    )

  res
}

#' @export
unique_tagged_na <- function(x, fromLast = FALSE) {
  x[!duplicated_tagged_na(x, fromLast = fromLast)]
}
