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

#' Unique elements, duplicated, ordering and sorting with tagged NAs
#'
#' These adapations of [base::unique()], [base::duplicated()],
#' [base::order()] and [base::sort()] treats tagged NAs as distinct
#' values.
#'
#' @param x a vector
#' @param fromLast logical indicating if duplication should be
#' considered from the last
#' @export
unique_tagged_na <- function(x, fromLast = FALSE) {
  x[!duplicated_tagged_na(x, fromLast = fromLast)]
}

#' @export
#' @rdname unique_tagged_na
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

#' @rdname unique_tagged_na
#' @param na.last if `TRUE`, missing values in the data are put last;
#' if `FALSE`, they are put first
#' @param decreasing should the sort order be increasing or decreasing?
#' @param method the method to be used, see [base::order()]
#' @param na_decreasing should the sort order for tagged NAs value be
#' @param untagged_na_last should untagged `NA`s be sorted after tagged `NA`s?
#' increasing or decreasing?
#' @export
order_tagged_na <- function(x, na.last = TRUE, decreasing = FALSE,
                            method = c("auto", "shell", "radix"),
                            na_decreasing = decreasing,
                            untagged_na_last = TRUE) {
  if (!is.double(x))
    return(
      order(x, na.last = na.last, decreasing = decreasing, method = method)
    )

  res <- order(
    x, na.last = TRUE, decreasing = decreasing, method = method
  )

  if (anyNA(x)) {
    n_na <- sum(is.na(x))
    if (n_na < length(x)) {
      res <- res[1:(length(x) - n_na)]
    } else {
      res <- NULL
    }

    t_na <- format_tagged_na(x)
    t_na[!is.na(x)] <- NA
    if (xor(untagged_na_last, na_decreasing))
      t_na[is.na(x) & !is_tagged_na(x)] <- "ZZZ"
    na_order <- order(
      t_na, na.last = TRUE, decreasing = na_decreasing, method = method
    )
    na_order <- na_order[1:n_na]

    if (na.last) res <- c(res, na_order)
    else res <- c(na_order, res)
  }

  res
}

#' @rdname unique_tagged_na
#' @export
sort_tagged_na <- function(x, decreasing = FALSE, na.last = TRUE,
                           na_decreasing = decreasing,
                           untagged_na_last = TRUE) {
  x[order_tagged_na(
    x,
    decreasing = decreasing,
    na.last = na.last,
    na_decreasing = na_decreasing,
    untagged_na_last = untagged_na_last
  )]
}
