#' Convert a dictionary data frame into a list of variable / value labels
#'
#' These helpers could be used to generate, from a data frame corresponding to
#' a variable dictionary, a list of variable or value labels that could be
#' passed to [set_variable_labels()] or to [set_value_labels()].
#'
#' It should be noted that value labels could be
#'
#' @param dictionary A data frame or a tibble containing the definitions of the
#' variable / value labels.
#' @param names_from <[`tidy-select`][dplyr::dplyr_tidy_select ]> Column
#' containing the names of the variables.
#' @param labels_from <[`tidy-select`][dplyr::dplyr_tidy_select ]> Column
#' containing the labels (unused if `delim_value_label` is provided).
#' @param values_from <[`tidy-select`][dplyr::dplyr_tidy_select ]> Column
#' containing the values or the pairs of values and labels (see examples).
#' @param delim_value_label Optional string giving the delimiter between the
#' value and the label.
#' @param delim_entries Optional string giving the delimiter between several
#' pairs of value and labels if provided in a unique row. `NULL` if each pair
#' is defined in a separate row (see examples).
#' @param data A data frame or a tibble containing the raw data to identify
#' the type (integer, numeric, character) of each variable.
#' @export
#' @examples
#' dic <- dplyr::tibble(
#'   variable = c("mpg", "vs", "am", "cyl"),
#'   label = c("miles / gallon", "Engine", "Transmission", NA),
#'   values = c(NA, "0:V-Shaped,1:straight", "0:automatic,1:manual", NA)
#' )
#' dic
#'
#' l <- dic %>% dictionary_to_variable_labels()
#' l
#' mtcars %>%
#'   set_variable_labels(.labels = l) %>%
#'   look_for()
#'
#' vl <- dic %>%
#'   dictionary_to_value_labels(
#'     values_from = values,
#'     delim_entries = ",",
#'     delim_value_label = ":",
#'     data = mtcars
#'   )
#' mtcars %>%
#'   set_value_labels(.labels = vl) %>%
#'   look_for()
#'
#' dic2 <- dplyr::tibble(
#'   variable = c("am", "am", "vs", "vs"),
#'   labels = c("0:automatic", "1:manual", "0:V-shaped", "1:straight")
#' )
#' dic2 %>%
#'   dictionary_to_value_labels(
#'     delim_value_label = ":",
#'     data = mtcars
#'   )
#'
#' dic3 <- dplyr::tibble(
#'   code = c(0, 1, 0, 1),
#'   label = c("automatic", "manual", "V-shaped", "straight"),
#'   var = c("am", "am", "vs", "vs")
#' )
#' dic3 %>%
#'   dictionary_to_value_labels(
#'     names_from = var,
#'     values_from = code,
#'     labels_from = label,
#'     data = mtcars
#'   )
dictionary_to_variable_labels <- function(dictionary,
                                          names_from = 1,
                                          labels_from = 2) {
  dictionary <-
    dictionary %>%
    tidyr::drop_na({{ names_from }})
  dictionary %>%
    dplyr::pull({{ labels_from }}) %>%
    as.list() %>%
    rlang::set_names(
      dictionary %>% dplyr::pull({{ names_from }})
    )
}

#' @rdname dictionary_to_variable_labels
#' @export
dictionary_to_value_labels <- function(dictionary,
                                       names_from = 1,
                                       values_from = 2,
                                       labels_from = 3,
                                       delim_entries = NULL,
                                       delim_value_label = NULL,
                                       data = NULL) {
  dictionary <-
    dictionary %>%
    tidyr::drop_na({{ names_from }}, {{ values_from }})
  if (!is.null(delim_entries))
    dictionary <-
      dictionary %>%
      tidyr::separate_longer_delim(
        cols = {{ values_from}},
        delim = delim_entries
      )
  if (!is.null(delim_value_label)) {
    dictionary <-
      dictionary %>%
      tidyr::separate_wider_delim(
        delim = delim_value_label,
        cols = {{ values_from }},
        names = c("..value..", "..label..")
      )
    values_from <- "..value.."
    labels_from <- "..label.."
  }
  d <- dictionary %>%
    dplyr::group_by(dplyr::pick({{ names_from }})) %>%
    dplyr::summarise(
      ..val_labels.. = purrr::map2(
        dplyr::pick({{ values_from }}),
        dplyr::pick({{ labels_from }}),
        ~ setNames(.x, .y)
      )
    )
  vl <- d %>%
    dplyr::pull("..val_labels..") %>%
    setNames(d %>% dplyr::pull({{ names_from }}))

  if (!is.null(data)) {
    for (v in intersect(names(vl), colnames(data))) {
      mode(vl[[v]]) <- mode(data[[v]])
    }
  }

  vl
}
