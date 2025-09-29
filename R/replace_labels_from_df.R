#' Replace Variable Labels in a Data Frame with Labels from A Lookup Data Frame
#'
#' If no replacement exists, keeps the original.
#'
#' @param data data frame for which labels will be replaced
#' @param label_df a data frame with a label_df_variable_col and a label_df_label_col
#' @param label_df_variable_col,label_df_label_col variable names (as strings) for the lookup of variable names in data, and the corresponding variable label.
#' @param overwrite Flag. Whether to remove label if it already exists.
#'
#' @return data, with variable labels replaced
#' @export
#'
#' @examples
#' # Create sample data
#' df <- data.frame(
#'   age = c(25, 30, 35),
#'   income = c(50000, 60000, 70000),
#'   education = c(1, 2, 3)
#' )
#'
#' # Create lookup data frame
#' labels_lookup <- data.frame(
#'   variable = c("age", "income", "education"),
#'   label = c("Age in years", "Annual income", "Education level")
#' )
#'
#' # Apply labels
#' df_labeled <- replace_labels_from_df(df, labels_lookup)
#' var_label(df_labeled$age) # "Age in years"
#'
#' # Control overwriting of existing labels
#' var_label(df$age) <- "Original label"
#'
#' # overwrite = FALSE preserves existing labels
#' df_preserve <- replace_labels_from_df(df, labels_lookup, overwrite = FALSE)
#' var_label(df_preserve$age) # "Original label"
#'
#' # overwrite = TRUE (default) replaces existing labels
#' df_replace <- replace_labels_from_df(df, labels_lookup, overwrite = TRUE)
#' var_label(df_replace$age) # "Age in years"
#'
#' # Custom column names in lookup
#' custom_lookup <- data.frame(
#'   var_name = c("age", "income"),
#'   var_label = c("Participant age", "Salary")
#' )
#'
#' df_custom <- replace_labels_from_df(
#'   df, custom_lookup,
#'   label_df_variable_col = "var_name",
#'   label_df_label_col = "var_label"
#' )
replace_labels_from_df <- function(data, label_df,
                                   label_df_variable_col = "variable",
                                   label_df_label_col = "label",
                                   overwrite = TRUE) {
  # Input validation using existing package patterns
  check_data_frame(data)
  check_data_frame(label_df)
  check_string(label_df_variable_col)
  check_string(label_df_label_col)
  check_bool(overwrite)

  # Validate that required columns exist in label_df
  if (!label_df_variable_col %in% names(label_df)) {
    stop(sprintf("Column '%s' not found in label_df", label_df_variable_col))
  }
  if (!label_df_label_col %in% names(label_df)) {
    stop(sprintf("Column '%s' not found in label_df", label_df_label_col))
  }

  # Convert lookup data frame to named list
  label_list <- stats::setNames(
    as.list(label_df[[label_df_label_col]]),
    label_df[[label_df_variable_col]]
  )

  # Filter to only string labels and existing variables
  if (length(label_list) > 0) {
    label_list <- label_list[
      sapply(label_list, rlang::is_string) &
        names(label_list) %in% names(data)
    ]
  }

  # If overwrite = FALSE, further filter to only variables without existing labels
  if (!overwrite && length(label_list) > 0) {
    existing_labels <- var_label(data, null_action = "skip")
    vars_with_labels <- names(existing_labels)[
      sapply(existing_labels, rlang::is_string)
    ]
    label_list <- label_list[!names(label_list) %in% vars_with_labels]
  }

  # Apply labels using existing set_variable_labels function
  if (length(label_list) > 0) {
    data <- set_variable_labels(data, .labels = label_list, .strict = FALSE)
  }

  data
}
