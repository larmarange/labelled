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

#' @export
order_tagged_na <- function(x, na.last = TRUE, decreasing = FALSE,
                            method = c("auto", "shell", "radix"),
                            na_decreasing = decreasing) {
  if (!is.double(x))
    return(
      order(x, na.last = na.last, decreasing = decreasing, method = method)
    )

  res <- order(
    x, na.last = TRUE, decreasing = decreasing, method = method
  )

  if (anyNA(x)) {
    n_na <- sum(is.na(x))
    res <- res[1:(length(x) - n_na)]

    t_na <- format_tagged_na(x)
    t_na[!is.na(x)] <- NA
    na_order <- order(
      t_na, na.last = TRUE, decreasing = na_decreasing, method = method
    )
    na_order <- na_order[1:n_na]

    if (na.last) res <- c(res, na_order)
    else res <- c(na_order, res)
  }

  res
}

#' @export
sort_tagged_na <- function(x, decreasing = FALSE, na.last = TRUE,
                           na_decreasing = decreasing) {
  x[order_tagged_na(
    x,
    decreasing = decreasing,
    na.last = na.last,
    na_decreasing = na_decreasing
  )]
}
