#' Copy variable and value labels and SPSS-style missing value
#'
#' This function copies variable and value labels (including missing values)
#' from one vector to another or from one data frame to another data frame.
#' For data frame, labels are copied according to variable names, and only
#' if variables are the same type in both data frames.
#'
#' Some base \R functions like [base::subset()] drop variable and
#' value labels attached to a variable. `copy_labels` could be used
#' to restore these attributes.
#'
#' `copy_labels_from` is intended to be used with \pkg{dplyr} syntax,
#' see examples.
#'
#' @param from A vector or a data.frame (or survey design) to copy labels from.
#' @param to A vector or data.frame (or survey design) to copy labels to.
#' @param .strict When `from` is a labelled vector, `to` have to be of the same
#' type (numeric or character) in order to copy value labels and SPSS-style
#' missing values. If this is not the case and `.strict = TRUE`, an error
#' will be produced. If `.strict = FALSE`, only variable label will be
#' copied.
#' @export
#' @examples
#' library(dplyr)
#' df <- tibble(
#'   id = 1:3,
#'   happy = factor(c("yes", "no", "yes")),
#'   gender = labelled(c(1, 1, 2), c(female = 1, male = 2))
#' ) %>%
#'   set_variable_labels(
#'     id = "Individual ID",
#'     happy = "Are you happy?",
#'     gender = "Gender of respondent"
#'   )
#' var_label(df)
#' fdf <- df %>% filter(id < 3)
#' var_label(fdf) # some variable labels have been lost
#' fdf <- fdf %>% copy_labels_from(df)
#' var_label(fdf)
#'
#' # Alternative syntax
#' fdf <- subset(df, id < 3)
#' fdf <- copy_labels(from = df, to = fdf)
copy_labels <- function(from, to, .strict = TRUE) {
  UseMethod("copy_labels")
}

#' @export
copy_labels.default <- function(from, to, .strict = TRUE) {
  var_label(to) <- var_label(from)
  to
}


#' @export
copy_labels.haven_labelled <- function(from, to, .strict = TRUE) {
  if (mode(from) != mode(to) && .strict)
    cli::cli_abort(
      paste(
        "{.arg from} ({class(from)}) and {.arg to} ({class(to)})",
        "must be of same type."
      )
    )
  var_label(to) <- var_label(from)

  if (mode(from) == mode(to)) {
    val_labels(to) <- val_labels(from)
    na_range(to) <- na_range(from)
    na_values(to) <- na_values(from)
  }
  to
}

#' @export
copy_labels.data.frame <- function(from, to, .strict = TRUE) {
  if (
    !inherits(to, "data.frame") &&
      !inherits(to, "survey.design") &&
      !inherits(to, "svyrep.design")
  )
    cli::cli_abort("{.arg to} should be a data frame or a survey design.")

  if (inherits(from, "survey.design") || inherits(from, "svyrep.design"))
    from <- from$variables

  if (inherits(to, "data.frame")) {
    for (var in names(to)) {
      if (var %in% names(from)) {
        to[[var]] <- copy_labels(from[[var]], to[[var]], .strict = .strict)
      }
    }
  } else { # survey design case
    for (var in names(to$variables)) {
      if (var %in% names(from)) {
        to$variables[[var]] <-
          copy_labels(from[[var]], to$variables[[var]], .strict = .strict)
      }
    }
  }

  to
}

#' @export
copy_labels.survey.design <- copy_labels.data.frame

#' @export
copy_labels.svyrep.design <- copy_labels.data.frame

#' @rdname copy_labels
#' @export
copy_labels_from <- function(to, from, .strict = TRUE) {
  copy_labels(from, to, .strict = .strict)
}
