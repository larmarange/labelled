#' Look for keywords variable names and descriptions / Create a data dictionary
#'
#' `look_for()` emulates the `lookfor` Stata command in \R. It supports
#' searching into the variable names of regular \R data frames as well as into
#' variable labels descriptions, factor levels and value labels.
#' The command is meant to help users finding variables in large datasets.
#'
#' When no keyword is provided, it will produce a data dictionary of the overall
#' data frame.
#'
#' @param data a data frame or a survey object
#' @param ... optional list of keywords, a character string (or several
#' character strings), which can be formatted as a regular expression suitable
#' for a [base::grep()] pattern, or a vector of keywords;
#' displays all variables if not specified
#' @param labels whether or not to search variable labels (descriptions);
#' `TRUE` by default
#' @param values whether or not to search within values (factor levels or value
#' labels); `TRUE` by default
#' @param ignore.case whether or not to make the keywords case sensitive;
#' `TRUE` by default (case is ignored during matching)
#' @param details add details about each variable (full details could be time
#' consuming for big data frames, `FALSE` is equivalent to `"none"`
#' and `TRUE` to `"full"`)
#' @param x a tibble returned by `look_for()`
#' @return a tibble data frame featuring the variable position, name and
#' description (if it exists) in the original data frame
#' @seealso `vignette("look_for")`
#' @details The function looks into the variable names for matches to the
#' keywords. If available, variable labels are included in the search scope.
#' Variable labels of data.frame imported with \pkg{foreign} or
#' \pkg{memisc} packages will also be taken into account (see [to_labelled()]).
#' If no keyword is provided, it will return all variables of `data`.
#'
#' `look_for()`, `lookfor()` and `generate_dictionary()` are equivalent.
#'
#' By default, results will be summarized when printing. To deactivate default
#' printing, use `dplyr::as_tibble()`.
#'
#' `lookfor_to_long_format()` could be used to transform results with one row
#' per factor level and per value label.
#'
#' Use `convert_list_columns_to_character()` to convert named list columns into
#' character vectors (see examples).
#'
#' The function `to_gt()` transform the results of `look_for()` into a nicely
#' formatted table using [gt::gt()]. This table could be easily be exported
#' to a file with [gt::gtsave()]. [gt::tab_header()] could be used to add a
#' title to the table (see examples).
#'
#' `look_for_and_select()` is a shortcut for selecting some variables and
#' applying [dplyr::select()]s to return a data frame with only the selected
#' variables.
#'
#' @author François Briatte <f.briatte@@gmail.com>,
#' Joseph Larmarange <joseph@@larmarange.net>
#' @examples
#' look_for(iris)
#'
#' # Look for a single keyword.
#' look_for(iris, "petal")
#' look_for(iris, "s")
#' iris %>%
#'   look_for_and_select("s") %>%
#'   head()
#'
#' # Look for with a regular expression
#' look_for(iris, "petal|species")
#' look_for(iris, "s$")
#'
#' # Look for with several keywords
#' look_for(iris, "pet", "sp")
#' look_for(iris, "pet", "sp", "width")
#' look_for(iris, "Pet", "sp", "width", ignore.case = FALSE)
#'
#' # Look_for can search within factor levels or value labels
#' look_for(iris, "vers")
#'
#' # Quicker search without variable details
#' look_for(iris, details = "none")
#'
#' # To obtain more details about each variable
#' look_for(iris, details = "full")
#'
#' # To deactivate default printing, convert to tibble
#' look_for(iris, details = "full") %>%
#'   dplyr::as_tibble()
#'
#' # To convert named lists into character vectors
#' look_for(iris) %>% convert_list_columns_to_character()
#'
#' # Long format with one row per factor and per value label
#' look_for(iris) %>% lookfor_to_long_format()
#'
#' # Both functions can be combined
#' look_for(iris) %>%
#'   lookfor_to_long_format() %>%
#'   convert_list_columns_to_character()
#'
#' # Labelled data
#' d <- dplyr::tibble(
#'   region = labelled_spss(
#'     c(1, 2, 1, 9, 2, 3),
#'     c(north = 1, south = 2, center = 3, missing = 9),
#'     na_values = 9,
#'     label = "Region of the respondent"
#'   ),
#'   sex = labelled(
#'     c("f", "f", "m", "m", "m", "f"),
#'     c(female = "f", male = "m"),
#'     label = "Sex of the respondent"
#'   )
#' )
#' look_for(d)
#' d %>%
#'   look_for() %>%
#'   lookfor_to_long_format() %>%
#'   convert_list_columns_to_character()
#' @source Inspired by the `lookfor` command in Stata.
#' @export

look_for <- function(data,
                     ...,
                     labels = TRUE,
                     values = TRUE,
                     ignore.case = TRUE,
                     details = c("basic", "none", "full")) {
  if (inherits(data, c("survey.design", "svyrep.design"))) {
    data <- data$variables
  }

  if (is.logical(details)) {
    details <- ifelse(details, "full", "none")
  } else {
    details <- match.arg(details)
  }
  # applying to_labelled
  data <- to_labelled(data)
  # search scope
  n <- names(data)
  if (!length(n))
    cli::cli_abort("There are no names to search in that object.")
  # search function
  keywords <- c(...)
  l <- unlist(var_label(data))
  if (!is.null(keywords)) {
    look <- function(x) {
      grep(paste(keywords, collapse = "|"), x, ignore.case = ignore.case)
    }
    # names search
    x <- look(n)
    variable <- n[x]
    # variable labels
    if (length(l) > 0 && labels) {
      # search labels
      y <- look(l)
      variable <- unique(c(variable, names(l[y])))
    }
    if (values) {
      # search factor levels
      fl <- lapply(data, levels)
      y <- look(fl)
      variable <- unique(c(variable, names(fl[y])))

      # search value levels
      vl <- lapply(data, val_labels)
      y <- look(vl)
      variable <- unique(c(variable, names(vl[y])))
    }
  } else {
    variable <- n
  }

  # output
  if (length(variable)) {
    pos <- which(n %in% variable)
    # reordering according to pos
    # not forgetting that some variables don't have a label
    if (length(l)) {
      res <- dplyr::tibble(pos = pos, variable = n[pos], label = l[n[pos]])
    } else {
      res <- dplyr::tibble(pos = pos, variable = n[pos], label = NA_character_)
    }

    if (details != "none") {
      data <- data %>%
        dplyr::select(dplyr::all_of(res$variable))

      n_missing <- function(x) {
        sum(is.na(x))
      }

      res <- res %>%
        dplyr::mutate(
          col_type = unlist(lapply(data, vctrs::vec_ptype_abbr)),
          missing = unlist(lapply(data, n_missing)),
          levels = lapply(data, levels),
          value_labels = lapply(data, val_labels),
        )
    }

    if (details == "full") {
      data <- data %>%
        dplyr::select(dplyr::all_of(res$variable))

      unique_values <- function(x) {
        length(unique(x))
      }
      generic_range <- function(x) {
        if (all(unlist(lapply(x, is.null)))) {
          return(NULL)
        }
        if (all(is.na(x))) {
          return(NULL)
        }

        r <- suppressWarnings(try(range(x, na.rm = TRUE), silent = TRUE))
        if (inherits(r, "try-error")) {
          return(NULL)
        }

        r
      }

      res <- res %>%
        dplyr::mutate(
          class = lapply(data, class),
          type = unlist(lapply(data, typeof)),
          na_values = lapply(data, na_values),
          na_range = lapply(data, na_range),
          n_na = missing, # retrocompatibility
          unique_values = unlist(lapply(data, unique_values)),
          range = lapply(data, generic_range)
        )
    }
  } else {
    res <- dplyr::tibble()
  }
  # add a look_for class
  class(res) <- c("look_for", class(res))
  res
}


#' @rdname look_for
#' @export
lookfor <- look_for

#' @rdname look_for
#' @export
generate_dictionary <- look_for


#' @rdname look_for
#' @export
print.look_for <- function(x, ...) {
  if (nrow(x) > 0 && all(c("pos", "variable", "label") %in% names(x))) {
    x <- x %>%
      lookfor_to_long_format() %>%
      convert_list_columns_to_character() %>%
      dplyr::mutate(
        # display -- when empty
        label = dplyr::if_else(is.na(.data$label), "\u2014", .data$label)
      )

    if (all(c("value_labels", "levels", "col_type") %in% names(x))) {
      if (!"range" %in% names(x)) {
        x$range <- NA_character_
      }
      x <- x %>%
        dplyr::mutate(
          values = dplyr::case_when(
            !is.na(.data$value_labels) ~ .data$value_labels,
            !is.na(.data$levels) ~ .data$levels,
            !is.na(.data$range) ~ paste("range:", .data$range),
            .default = "" # zero-width space
          ),
          variable = dplyr::if_else(
            duplicated(.data$pos),
            "",
            .data$variable
          ),
          label = dplyr::if_else(duplicated(.data$pos), "", .data$label),
          col_type = dplyr::if_else(duplicated(.data$pos), "", .data$col_type),
        )

      if ("missing" %in% names(x)) {
        x <- x %>%
          dplyr::mutate(
            missing = dplyr::if_else(
              duplicated(.data$pos),
              "",
              as.character(.data$missing)
            )
          )
      }

      if ("unique_values" %in% names(x)) {
        x <- x %>%
          dplyr::mutate(
            unique_values = dplyr::if_else(
              duplicated(.data$pos),
              "",
              as.character(.data$unique_values)
            )
          )
      }

      if ("na_values" %in% names(x)) {
        x <- x %>%
          dplyr::mutate(
            na_values = dplyr::if_else(
              duplicated(.data$pos),
              "",
              as.character(.data$na_values)
            )
          )
      }

      if ("na_range" %in% names(x)) {
        x <- x %>%
          dplyr::mutate(
            na_range = dplyr::if_else(
              duplicated(.data$pos),
              "",
              as.character(.data$na_range)
            )
          )
      }

      x <- x %>%
        dplyr::mutate(
          pos = dplyr::if_else(
            duplicated(.data$pos),
            "",
            as.character(.data$pos)
          )
        ) %>%
        dplyr::select(
          dplyr::any_of(c(
            "pos", "variable", "label", "col_type", "missing",
            "unique_values", "values", "na_values", "na_range"
          ))
        )
    }
    w <- getOption("width") # available width for printing
    w_pos <- max(3, stringr::str_length(x$pos))
    w_variable <- max(5, stringr::str_length(x$variable))
    w_label <- max(5, stringr::str_length(x$label)) # nolint

    if ("values" %in% names(x)) {
      w_col_type <- max(8, stringr::str_length(x$col_type))
      w_values <- max(5, stringr::str_length(x$values)) # nolint
      w_missing <- max(7, stringr::str_length(x$missing))
      # width for labels
      lw <- w - 8 - w_pos - w_variable - w_col_type - w_missing
      lw <- dplyr::case_when(
        w_values < lw / 2 ~ lw - w_values,
        w_label < lw / 2 ~ lw - w_label,
        .default = trunc(lw / 2)
      )
      # a minimum of 10
      lw <- max(10, lw)
      x$label <- stringr::str_trunc(x$label, lw, ellipsis = "~")
      x$values <- stringr::str_trunc(x$values, lw, ellipsis = "~")
    } else {
      lw <- w - 4 - w_pos - w_variable
      lw <- max(10, lw)
      x$label <- stringr::str_trunc(x$label, lw, ellipsis = "~")
    }

    print.data.frame(x, row.names = FALSE, quote = FALSE, right = FALSE)
  } else if (nrow(x) == 0) {
    cli::cli_alert_warning("Nothing found. Sorry.")
  } else {
    print(dplyr::as_tibble(x))
  }
}

#' @rdname look_for
#' @export
look_for_and_select <- function(
    data,
    ...,
    labels = TRUE,
    values = TRUE,
    ignore.case = TRUE) {
  lf <- data %>%
    look_for(
      ...,
      labels = labels,
      values = values,
      ignore.case = ignore.case,
      details = "none"
    )
  data %>% dplyr::select(lf$pos)
}

#' @rdname look_for
#' @export
convert_list_columns_to_character <- function(x) {
  if ("range" %in% names(x)) {
    x <- x %>%
      dplyr::mutate(range = unlist(lapply(range, paste, collapse = " - ")))
  }

  if ("value_labels" %in% names(x) && is.list(x$value_labels)) {
    x <- x %>%
      dplyr::mutate(value_labels = names_prefixed_by_values(.data$value_labels))
  }

  x %>%
    dplyr::as_tibble() %>% # remove look_for class
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.list),
        ~ unlist(lapply(.x, paste, collapse = "; "))
      )
    )
}

#' @rdname look_for
#' @export
lookfor_to_long_format <- function(x) {
  # only if details are provided
  if (!"levels" %in% names(x) || !"value_labels" %in% names(x)) {
    return(x)
  }

  x <- x %>%
    dplyr::as_tibble() %>% # remove look_for class
    dplyr::mutate(value_labels = names_prefixed_by_values(.data$value_labels))

  # tidyr::unnest() fails if all elements are NULL
  if (all(unlist(lapply(x$levels, is.null)))) {
    x$levels <- NA_character_
  }
  if (all(unlist(lapply(x$value_labels, is.null)))) {
    x$value_labels <- NA_character_
  }

  x %>%
    tidyr::unnest("levels", keep_empty = TRUE) %>%
    tidyr::unnest("value_labels", keep_empty = TRUE)
}

#' @rdname look_for
#' @param column_labels Optional column labels
#' @export
#' @examplesIf rlang::is_installed("gt")
#' iris %>% look_for(details = TRUE) %>% to_gt()
#' d %>%
#'   generate_dictionary() %>%
#'   to_gt() %>%
#'   gt::tab_header(gt::md("**Variable dictionary**"))
to_gt <- function(
  x,
  column_labels = list(
    pos = "#",
    variable = "Variable",
    col_type = "Type",
    label = "Variable label",
    values = "Values",
    missing = "Missing values",
    unique_values = "Unique values",
    na_values = "User-defined missings (values)",
    na_range = "User-defined missings (range)"
  )
) {
  rlang::check_installed("gt")
  if (!inherits(x, "look_for"))
    cli::cli_abort("{.arg x} shoud be a {.class look_for} object.")

  x <-
    x %>%
    dplyr::mutate(
      label = dplyr::if_else(is.na(.data$label), "\u2014", .data$label)
    )
  if ("levels" %in% names(x)) {
    x <-
      x %>%
      dplyr::mutate(
        levels = purrr::map(
          .data$levels,
          function(x) {
            if (is.null(x)) return("")
            paste("-", x, collapse = "\n") |>
              gt::md()
          }
        )
      )
  }
  if ("value_labels" %in% names(x)) {
    x <-
      x %>%
      dplyr::mutate(
        value_labels = purrr::map(
          .data$value_labels,
          function(x) {
            if (is.null(x)) return("")
            paste("-", names_prefixed_by_values(x), collapse = "\n") |>
              gt::md()
          }
        )
      )
  }
  if ("range" %in% names(x)) {
    x <-
      x %>%
      dplyr::mutate(
        range = purrr::map(
          .data$range,
          function(x) {
            if (is.null(x)) return("")
            paste(x, collapse = " – ")
          }
        )
      )
  } else {
    x$range <- ""
    x$range <- as.list(x$range)
  }
  if (all(c("levels", "value_labels") %in% names(x))) {
    x$values <-
      dplyr::case_when(
        x$value_labels != "" ~ x$value_labels,
        x$levels != "" ~ x$levels,
        TRUE ~ x$range
      )
  }

  keep <- c(
    "pos", "variable", "col_type", "label",
    "values", "missing", "unique_values"
  )

  if ("na_values" %in% names(x) && !is.null(unlist(x$na_values)))
    keep <- c(keep, "na_values")
  if ("na_range" %in% names(x) && !is.null(unlist(x$na_range)))
    keep <- c(keep, "na_range")


  tbl <-
    x %>%
    dplyr::select(
      dplyr::any_of(keep)
    ) %>%
    set_variable_labels(.labels = column_labels, .strict = FALSE) %>%
    gt::gt() %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(column = "variable")
    )

  if ("values" %in% names(x)) {
    tbl <-
      tbl %>%
      gt::tab_style(
        style = gt::cell_text(align = "left"),
        locations = gt::cells_body(
          columns = "values",
          rows = x$values %>%
            purrr::map(~ inherits(.x, "from_markdown")) %>%
            unlist()
        )
      )
  }

  tbl %>%
    gt::tab_style(
      style = "vertical-align:top",
      locations = gt::cells_body()
    ) %>%
    gt::tab_style(
      style = "vertical-align:top",
      locations = gt::cells_column_labels()
    )
}
