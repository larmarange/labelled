#' Convert input to a character vector
#'
#' By default, `to_character()` is a wrapper for [base::as.character()].
#' For labelled vector, to_character allows to specify if value, labels or
#' labels prefixed with values should be used for conversion.
#'
#' @param x Object to coerce to a character vector.
#' @param ... Other arguments passed down to method.
#' @param explicit_tagged_na should tagged NA be kept?
#' @export
to_character <- function(x, ...) {
  UseMethod("to_character")
}

#' @export
to_character.default <- function(x, ...) {
  vl <- var_label(x)
  x <- as.character(x)
  var_label(x) <- vl
  x
}

#' @rdname to_character
#' @export
to_character.double <- function(x, explicit_tagged_na = FALSE, ...) {
  res <- as.character(x)
  if (explicit_tagged_na) {
    res[is_tagged_na(x)] <- format_tagged_na(x[is_tagged_na(x)])
  }
  var_label(res) <- var_label(x)
  names(res) <- names(x)
  res
}

#' @rdname to_character
#' @param levels What should be used for the factor levels: the labels, the
#' values or labels prefixed with values?
#' @param nolabel_to_na Should values with no label be converted to `NA`?
#' @param user_na_to_na user defined missing values into NA?
#' @details
#'   If some values doesn't have a label, automatic labels will be created,
#'   except if `nolabel_to_na` is `TRUE`.
#' @examples
#' v <- labelled(
#'   c(1, 2, 2, 2, 3, 9, 1, 3, 2, NA),
#'   c(yes = 1, no = 3, "don't know" = 9)
#' )
#' to_character(v)
#' to_character(v, nolabel_to_na = TRUE)
#' to_character(v, "v")
#' to_character(v, "p")
#' @export
to_character.haven_labelled <- function(
    x,
    levels = c("labels", "values", "prefixed"),
    nolabel_to_na = FALSE,
    user_na_to_na = FALSE,
    explicit_tagged_na = FALSE,
    ...) {
  vl <- var_label(x)
  levels <- match.arg(levels)
  x <- as.character(to_factor(
    x,
    levels = levels, nolabel_to_na = nolabel_to_na,
    user_na_to_na = user_na_to_na, explicit_tagged_na = explicit_tagged_na
  ))
  var_label(x) <- vl
  x
}


#' @rdname to_character
#' @param labelled_only for a data.frame, convert only labelled variables to
#' factors?
#' @details
#'   When applied to a data.frame, only labelled vectors are converted by
#'   default to character. Use `labelled_only = FALSE` to convert all variables
#'   to characters.
#' @export
#' @examples
#'
#' df <- data.frame(
#'   a = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2)),
#'   b = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2, DK = 3)),
#'   c = labelled(
#'     c("a", "a", "b", "c"),
#'     labels = c(No = "a", Maybe = "b", Yes = "c")
#'   ),
#'   d = 1:4,
#'   e = factor(c("item1", "item2", "item1", "item2")),
#'   f = c("itemA", "itemA", "itemB", "itemB"),
#'   stringsAsFactors = FALSE
#' )
#'
#' if (require(dplyr)) {
#'   glimpse(df)
#'   glimpse(to_character(df))
#'   glimpse(to_character(df, labelled_only = FALSE))
#' }
to_character.data.frame <- function(
    x,
    levels = c("labels", "values", "prefixed"),
    nolabel_to_na = FALSE,
    user_na_to_na = FALSE,
    explicit_tagged_na = FALSE,
    labelled_only = TRUE,
    ...) {
  cl <- class(x)
  x <- dplyr::as_tibble(
    lapply(
      x,
      .to_character_col_data_frame,
      levels = levels,
      nolabel_to_na = nolabel_to_na,
      user_na_to_na = user_na_to_na,
      explicit_tagged_na = explicit_tagged_na,
      labelled_only = labelled_only,
      ...
    )
  )
  class(x) <- cl
  x
}

.to_character_col_data_frame <- function(
    x,
    levels = c("labels", "values", "prefixed"),
    nolabel_to_na = FALSE,
    user_na_to_na = FALSE,
    explicit_tagged_na = FALSE,
    labelled_only = TRUE,
    ...) {
  if (inherits(x, "haven_labelled")) {
    x <- to_character(x,
      levels = levels,
      nolabel_to_na = nolabel_to_na,
      user_na_to_na = user_na_to_na,
      explicit_tagged_na = explicit_tagged_na,
      ...
    )
  } else if (!labelled_only) {
    x <- to_character(x)
  }
  x
}

#' @export
#' @rdname to_character
to_character.survey.design <- function(
    x,
    levels = c("labels", "values", "prefixed"),
    nolabel_to_na = FALSE,
    user_na_to_na = FALSE,
    explicit_tagged_na = FALSE,
    labelled_only = TRUE,
    ...) {
  x$variables <-
    to_character(
      x$variables,
      levels = levels,
      nolabel_to_na = nolabel_to_na,
      user_na_to_na = user_na_to_na,
      explicit_tagged_na = explicit_tagged_na,
      labelled_only = labelled_only,
      ...
    )
  x
}

#' @export
to_character.svyrep.design <- to_character.survey.design
