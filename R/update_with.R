#' Update variable/value labels with a function
#' @param .data A data frame, or data frame extension (e.g. a tibble)
#' @param .fn A function used to transform the variable/value labels of the
#' selected `.cols`.
#' @param .cols Columns to update; defaults to all columns. Use tidy selection.
#' @param ... additional arguments passed onto `.fn`.
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
  vl <- var_label(.data)
  vl <- vl[names(cols)]
  vl <- lapply(vl, .fn, ...)
  var_label(.data) <- vl
  .data
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
