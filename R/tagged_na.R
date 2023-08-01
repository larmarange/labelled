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
#' These adaptations of [base::unique()], [base::duplicated()],
#' [base::order()] and [base::sort()] treats tagged NAs as distinct
#' values.
#'
#' @param x a vector
#' @param fromLast logical indicating if duplication should be
#' considered from the last
#' @export
#' @examples
#' x <- c(1, 2, tagged_na("a"), 1, tagged_na("z"), 2, tagged_na("a"), NA)
#' x %>% print_tagged_na()
#'
#' unique(x) %>% print_tagged_na()
#' unique_tagged_na(x) %>% print_tagged_na()
#'
#' duplicated(x)
#' duplicated_tagged_na(x)
#'
#' order(x)
#' order_tagged_na(x)
#'
#' sort(x, na.last = TRUE) %>% print_tagged_na()
#' sort_tagged_na(x) %>% print_tagged_na()
unique_tagged_na <- function(x, fromLast = FALSE) {
  x[!duplicated_tagged_na(x, fromLast = fromLast)]
}

#' @export
#' @rdname unique_tagged_na
duplicated_tagged_na <- function(x, fromLast = FALSE) {
  if (!is.double(x)) {
    return(duplicated(x, fromLast = fromLast))
  }

  res <- duplicated(x, fromLast = fromLast, incomparables = NA)

  if (anyNA(x)) {
    res[is.na(x)] <- duplicated(
      format_tagged_na(x[is.na(x)]),
      fromLast = fromLast
    )
  }

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
  if (!is.double(x)) {
    return(
      order(x, na.last = na.last, decreasing = decreasing, method = method)
    )
  }

  res <- order(
    x,
    na.last = TRUE, decreasing = decreasing, method = method
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
    if (xor(untagged_na_last, na_decreasing)) {
      t_na[is.na(x) & !is_tagged_na(x)] <- "ZZZ"
    }
    na_order <- order(
      t_na,
      na.last = TRUE, decreasing = na_decreasing, method = method
    )
    na_order <- na_order[1:n_na]

    if (na.last) {
      res <- c(res, na_order)
    } else {
      res <- c(na_order, res)
    }
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


#' Convert tagged NAs into user NAs
#'
#' [tagged_na_to_user_na()] is the opposite of [user_na_to_tagged_na()] and
#' convert tagged `NA` into user defined missing values (see [labelled_spss()]).
#'
#' [tagged_na_to_regular_na()] converts tagged NAs into regular NAs.
#'
#' @param x a vector or a data frame
#' @param user_na_start minimum value of the new user na, if `NULL`,
#' computed automatically (maximum of observed values + 1)
#' @export
#' @examples
#' x <- c(1:5, tagged_na("a"), tagged_na("z"), NA)
#' x
#' print_tagged_na(x)
#' tagged_na_to_user_na(x)
#' tagged_na_to_user_na(x, user_na_start = 10)
#'
#' y <- c(1, 0, 1, tagged_na("r"), 0, tagged_na("d"))
#' val_labels(y) <- c(
#'   no = 0, yes = 1,
#'   "don't know" = tagged_na("d"),
#'   refusal = tagged_na("r")
#' )
#' y
#' tagged_na_to_user_na(y, user_na_start = 8)
#' tagged_na_to_regular_na(y)
#' tagged_na_to_regular_na(y) %>% is_tagged_na()
tagged_na_to_user_na <- function(x, user_na_start = NULL) {
  UseMethod("tagged_na_to_user_na")
}

#' @export
tagged_na_to_user_na.default <- function(x, user_na_start = NULL) {
  # do nothing
  x
}

#' @export
tagged_na_to_user_na.double <- function(x, user_na_start = NULL) {
  if (is.null(user_na_start)) {
    user_na_start <- trunc(max(x, na.rm = TRUE)) + 1
  }
  tn <- x[is_tagged_na(x)] %>%
    unique_tagged_na() %>%
    sort_tagged_na()
  if (length(tn) == 0) {
    return(x)
  }
  labels <- val_labels(x)
  for (i in seq_along(tn)) {
    new_val <- user_na_start + i - 1
    if (any(x == new_val, na.rm = TRUE)) {
      stop(
        "Value ",
        new_val,
        " is already used in 'x'. Please change 'user_na_start'."
      )
    }
    x[is_tagged_na(x, na_tag(tn[i]))] <- new_val
    if (any(is_tagged_na(labels, na_tag(tn[i])), na.rm = TRUE)) {
      labels[is_tagged_na(labels, na_tag(tn[i]))] <- new_val
    } else {
      names(new_val) <- format_tagged_na(tn[i])
      labels <- c(labels, new_val)
    }
  }
  if (length(labels) > 0) {
    val_labels(x) <- labels
  }
  na_range(x) <- c(user_na_start, user_na_start + length(tn) - 1)
  x
}

#' @export
tagged_na_to_user_na.data.frame <- function(x, user_na_start = NULL) {
  x[] <- lapply(x, tagged_na_to_user_na, user_na_start = user_na_start)
  x
}

#' @rdname tagged_na_to_user_na
#' @export
tagged_na_to_regular_na <- function(x) {
  UseMethod("tagged_na_to_regular_na")
}

#' @export
tagged_na_to_regular_na.default <- function(x) {
  # do nothing
  x
}

#' @export
tagged_na_to_regular_na.double <- function(x) {
  x[is_tagged_na(x)] <- NA
  # removing value labels attached to tagged NAs, if any
  vl <- val_labels(x)
  if (any(is_tagged_na(vl))) {
    val_labels(x) <- vl[!is_tagged_na(vl)]
  }
  x
}

#' @export
tagged_na_to_regular_na.data.frame <- function(x) {
  x[] <- lapply(x, tagged_na_to_regular_na)
  x
}
