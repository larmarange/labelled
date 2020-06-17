#' Copy variable and value labels and SPSS style missing value
#'
#' This function copies variable and value labels (including missing values)
#' from one vector to another or from one data frame to another data frame.
#' For data frame, labels are copied according to variable names, and only
#' if variables are the same type in both data frames.
#'
#' Some base \R functions like [base::subset()] drop variable and
#' value labels attached to a variable. `copy_labels` coud be used
#' to restore these attributes.
#'
#' `copy_labels_from` is intended to be used with \pkg{dplyr} syntax,
#' see examples.
#'
#' @param from A vector or a data.frame (or tibble) to copy labels from.
#' @param to A vector or data.frame (or tibble) to copy labels to.
#' @export
#' @examples
#' library(dplyr)
#' df <- tibble(
#'   id = 1:3,
#'   happy = factor(c('yes', 'no', 'yes')),
#'   gender = labelled(c(1, 1, 2), c(female = 1, male = 2))
#' ) %>%
#' set_variable_labels(
#'   id = "Individual ID",
#'   happy = "Are you happy?",
#'   gender = "Gender of respondent"
#' )
#' var_label(df)
#' fdf <- df %>% filter(id < 3)
#' var_label(fdf) # some variable labels have been lost
#' fdf <- fdf %>% copy_labels_from(df)
#' var_label(fdf)
#'
#' # Alternative syntax
#' fdf <- subset(df, id < 3)
#' fdf <- copy_labels(from = df, to = fdf)
copy_labels <- function(from, to) {
  UseMethod("copy_labels")
}


#' @export
copy_labels.default <- function(from, to) {
  if (!is.atomic(from) | !is.atomic(to))
    stop("`from` and `to` should be vectors", call. = FALSE,
      domain = "R-labelled")
  if (mode(from) != mode(to))
    stop("`from` and `to` should be of same type", call. = FALSE,
      domain = "R-labelled")
  var_label(to) <- var_label(from)
  if (!is.factor(to)) {
    val_labels(to) <- val_labels(from)
    na_range(to) <- na_range(from)
    na_values(to) <- na_values(from)
  }
  to
}

#' @export
copy_labels.data.frame <- function(from, to) {
  if (!is.data.frame(to))
    stop("`to` should be a data frame", call. = FALSE, domain = "R-labelled")
  for (var in names(to)) if (var %in% names(from))
    to[[var]] <- copy_labels(from[[var]], to[[var]])
  to
}

#' @rdname copy_labels
#' @export
copy_labels_from <- function(to, from) {
  copy_labels(from, to)
}
