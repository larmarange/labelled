#' Update variable/value labels with a function
#' @param .data A data frame, adata frame extension (e.g. a tibble) or
#' a survey design
#' @param .fn A function used to transform the variable/value labels of the
#' selected `.cols`.
#' @param .cols Columns to update; defaults to all columns. Use tidy selection.
#' @param ... additional arguments passed onto `.fn`.
#' @details
#' For `update_variable_labels_with()`, it is possible to access the name of
#' the variable inside `.fn` by using `names()`, i.e. `.fn` receive a named
#' character vector (see example). `.fn` can return `as.character(NA)` to
#' remove a variable label.
#' @examples
#' df <- iris %>%
#'   set_variable_labels(
#'     Sepal.Length = "Length of sepal",
#'     Sepal.Width = "Width of sepal",
#'     Petal.Length = "Length of petal",
#'     Petal.Width = "Width of petal",
#'     Species = "Species"
#'   )
#' df$Species <- to_labelled(df$Species)
#' df %>% look_for()
#' df %>%
#'   update_variable_labels_with(toupper) %>%
#'   look_for()
#'
#' # accessing variable names with names()
#' df %>%
#'   update_variable_labels_with(function(x){tolower(names(x))}) %>%
#'   look_for()
#'
#' df %>%
#'   update_variable_labels_with(toupper, .cols = dplyr::starts_with("S")) %>%
#'   look_for()
#' @export
update_variable_labels_with <- function(.data,
                                        .fn,
                                        .cols = dplyr::everything(),
                                        ...) {
  UseMethod("update_variable_labels_with")
}

#' @export
update_variable_labels_with.data.frame <- function(.data,
                                                   .fn,
                                                   .cols = dplyr::everything(),
                                                   ...) {
  .fn <- rlang::as_function(.fn)
  cols <- tidyselect::eval_select(
    rlang::enquo(.cols),
    .data,
    allow_rename = FALSE
  )
  vl <- var_label(.data, null_action = "na")
  vl <- vl[names(cols)]

  vl <- mapply(
    function(variable, label) {
      stats::setNames(label, variable)
    },
    names(vl),
    vl,
    SIMPLIFY = FALSE
  )
  vl <- lapply(vl, .fn, ...)
  vl <- lapply(vl, unname)
  var_label(.data) <- vl
  .data
}

#' @export
update_variable_labels_with.survey.design <- function(
  .data,
  .fn,
  .cols = dplyr::everything(),
  ...
) {
  update_variable_labels_with(
    .data$variables,
    .fn = .fn,
    .cols = {{ .cols }},
    ...
  )
}

#' @export
#' @rdname update_variable_labels_with
#' @examples
#' df %>%
#'   update_value_labels_with(toupper) %>%
#'   look_for()
update_value_labels_with <- function(.data,
                                     .fn,
                                     .cols = dplyr::everything(),
                                     ...) {
  UseMethod("update_value_labels_with")
}

#' @export
update_value_labels_with.data.frame <- function(.data,
                                                .fn,
                                                .cols = dplyr::everything(),
                                                ...) {
  .fn <- rlang::as_function(.fn)
  cols <- tidyselect::eval_select(
    rlang::enquo(.cols),
    .data,
    allow_rename = FALSE
  )
  for (i in cols) {
    vl <- val_labels(.data[[i]])
    if (!is.null(vl)) {
      names(vl) <- .fn(names(vl), ...)
      val_labels(.data[[i]]) <- vl
    }
  }
  .data
}

#' @export
update_value_labels_with.survey.design <- function(
  .data,
  .fn,
  .cols = dplyr::everything(),
  ...
) {
  update_value_labels_with(
    .data$variables,
    .fn = .fn,
    .cols = {{ .cols }},
    ...
  )
}
