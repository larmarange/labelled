#' Look for keywords variable names and descriptions / Create a data dictionary
#'
#' `look_for` emulates the `lookfor` Stata command in \R. It supports
#' searching into the variable names of regular \R data frames as well as into
#' variable labels descriptions.
#' The command is meant to help users finding variables in large datasets.
#'
#' When no keyword is provided, it will produce a data dictionary of the overall
#' data frame.
#'
#' @param data a data frame
#' @param ... optional list of keywords, a character string (or several character strings), which can be
#' formatted as a regular expression suitable for a [base::grep()] pattern, or a vector of keywords;
#' displays all variables if not specified
#' @param labels whether or not to search variable labels (descriptions); `TRUE` by default
#' @param ignore.case whether or not to make the keywords case sensitive;
#' `TRUE` by default (case is ignored during matching)
#' @param details add details about each variable (turn off for a quicker search)
#' @param x a tibble returned by `look_for()`
#' @return a tibble data frame featuring the variable position, name and description
#' (if it exists) in the original data frame
#' @details The function looks into the variable names for matches to the keywords. If available,
#' variable labels are included in the search scope.
#' Variable labels of data.frame imported with \pkg{foreign} or
#' \pkg{memisc} packages will also be taken into account (see [to_labelled()]). If no keyword is
#' provided, it will return all variables of `data`.
#'
#' `look_for()`, `lookfor()` and `generate_dictionary()` are equivalent.
#'
#' By default, results will be summrized when printing. To deactivate default printing,
#' use `dplyr::as_tibble()`.
#'
#' `lookfor_to_long_format()` could be used to transform results with one row per factor level
#' and per value label.
#'
#' Use `convert_list_columns_to_character()` to convert named list columns into character vectors
#' (see examples).
#'
#' @author François Briatte <f.briatte@@gmail.com>, Joseph Larmarange <joseph@@larmarange.net>
#' @examples
#' look_for(iris)
#'
#' # Look for a single keyword.
#' look_for(iris, "petal")
#' look_for(iris, "s")
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
#' # Quicker search without variable details
#' look_for(iris, details = TRUE)
#'
#' # To deactivate default printing, convert to tibble
#' look_for(iris) %>% dplyr::as_tibble()
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
#' \dontrun{
#'   data(fertility, package = "questionr")
#'   look_for(children)
#'   look_for(children, "id")
#'   look_for(children) %>%
#'     lookfor_to_long_format() %>%
#'     convert_list_columns_to_character()
#' }
#' @source Based on the behaviour of the `lookfor` command in Stata.
#' @export

look_for <- function(data,
                    ...,
                    labels = TRUE,
                    ignore.case = TRUE,
                    details = TRUE) {
  # applying to_labelled
  data <- to_labelled(data)
  # search scope
  n <- names(data)
  if(!length(n)) stop("there are no names to search in that object")
  # search function
  keywords <- c(...)
  if(is.null(keywords)) keywords <- names(data)
  look <- function(x) { grep(paste(keywords, collapse="|"), x, ignore.case = ignore.case) }
  # names search
  x <- look(n)
  variable <- n[x]
  # variable labels
  l <- unlist(var_label(data))
  if(length(l) > 0 & labels) {
    # search labels
    y <- look(l)
    variable <- unique(c(variable, names(l[y])))
  }

  # output
  if(length(variable)) {
    pos <- which(n %in% variable)
    # reordering according to pos
    # not forgetting that some variables don't have a label
    if (length(l)) {
      res <- dplyr::tibble(pos = pos, variable = n[pos], label = l[n[pos]])
    } else {
      res <- dplyr::tibble(pos = pos, variable = n[pos], label = NA_character_)
    }

    if (details) {
      data <- data %>%
        dplyr::select(res$variable)

      unique_values <- function(x) {length(unique(x))}
      n_na <- function(x) {sum(is.na(x))}

      generic_range <- function(x){
        if (all(unlist(lapply(x, is.null)))) return(NULL)
        if (all(is.na(x))) return(NULL)

        r <- suppressWarnings(try(range(x, na.rm = TRUE), silent = TRUE))
        if (inherits(r, "try-error")) return(NULL)

        r
      }

      res <- res %>%
        dplyr::mutate(
          col_type = unlist(lapply(data, vctrs::vec_ptype_abbr)),
          class = lapply(data, class),
          type = unlist(lapply(data, typeof)),
          levels = lapply(data, levels),
          value_labels = lapply(data, val_labels),
          na_values = lapply(data, na_values),
          na_range = lapply(data, na_range),
          unique_values = unlist(lapply(data, unique_values)),
          n_na = unlist(lapply(data, n_na)),
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
  if (nrow(x) > 0) {
    x <- x %>%
      lookfor_to_long_format() %>%
      convert_list_columns_to_character() %>%
      dplyr::mutate(
        label = dplyr::if_else(is.na(.data$label), "\u2014", .data$label) # display -- when empty
      )

    if (all(c("value_labels", "levels", "range", "col_type") %in% names(x))) {
      x <- x %>%
        dplyr::mutate(
          values = dplyr::case_when(
            !is.na(.data$value_labels) ~ .data$value_labels,
            !is.na(.data$levels) ~ .data$levels,
            !is.na(.data$range) ~ paste("range:", .data$range),
            TRUE ~ "\u200b" # zero-width space
          ),
          variable = dplyr::if_else(duplicated(.data$pos), "\u200b", .data$variable),
          label = dplyr::if_else(duplicated(.data$pos), "\u200b", .data$label),
          col_type = dplyr::if_else(duplicated(.data$pos), "\u200b", .data$col_type),
          pos = dplyr::if_else(duplicated(.data$pos), "\u200b", as.character(.data$pos))
        ) %>%
        dplyr::select(dplyr::any_of(c("pos", "variable", "label", "col_type", "values")))
    }
    print(pillar::colonnade(x, has_row_id = FALSE))
  } else {
    message("Nothing found. Sorry.")
  }
}

#' @rdname look_for
#' @export
convert_list_columns_to_character <- function(x) {
  if ("range" %in% names(x))
    x <- x %>%
      dplyr::mutate(range = unlist(lapply(range, paste, collapse = " - ")))

  x %>%
    dplyr::as_tibble() %>% # remove look_for class
    dplyr::mutate(
      dplyr::across(where(is.list), ~ unlist(lapply(.x, paste, collapse = "; ")))
    )
}

#' @rdname look_for
#' @export
lookfor_to_long_format <- function(x) {
  # only if details are provided
  if (!"levels" %in% names(x) | !"value_labels" %in% names(x))
    return(x)

  x <- x %>%
    dplyr::as_tibble() %>% # remove look_for class
    dplyr::mutate(value_labels = names_prefixed_by_values(.data$value_labels))

  # tidyr::unnest() fails if all elements are NULL
  if (all(unlist(lapply(x$levels, is.null))))
    x$levels <- NA_character_
  if (all(unlist(lapply(x$value_labels, is.null))))
    x$value_labels <- NA_character_

  x %>%
    tidyr::unnest("levels", keep_empty = TRUE) %>%
    tidyr::unnest("value_labels", keep_empty = TRUE)
}


