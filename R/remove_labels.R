#' Remove variable label, value labels and user defined missing values
#'
#' Use `remove_var_label()` to remove variable label, `remove_val_labels()`
#' to remove value labels, `remove_user_na()` to remove user defined missing values (*na_values* and *na_range*)
#' and `remove_labels()` to remove all.
#'
#' @param x A vector or a data frame.
#' @param user_na_to_na Convert user defined missing values into `NA`?
#' @param user_na_to_tagged_na Convert user defined missing values into tagged `NA`?
#' It could be applied only to numeric vectors. Note that integer labelled vectors
#' will be converted to double labelled vectors.
#' @param keep_var_label Keep variable label?
#' @details
#' Be careful with `remove_user_na()` and `remove_labels()`, user defined missing values
#' will not be automatically converted to `NA`, except if you specify
#' `user_na_to_na = TRUE`.
#' `user_na_to_na(x)` is an equivalent of `remove_user_na(x, user_na_to_na = TRUE)`.
#'
#' If you prefer to convert variables with value labels into factors, use [to_factor()]
#' or use [unlabelled()].
#' @examples
#' x1 <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
#' var_label(x1) <- "A variable"
#' x1
#'
#' x2 <- remove_labels(x1)
#' x2
#' x3 <- remove_labels(x1, user_na_to_na = TRUE)
#' x3
#' x4 <- remove_user_na(x1, user_na_to_na = TRUE)
#' x4
#' @export
remove_labels <- function(x,
                          user_na_to_na = FALSE,
                          keep_var_label = FALSE,
                          user_na_to_tagged_na = FALSE
                          ) {
  UseMethod("remove_labels")
}


#' @export
remove_labels.default <- function(x,
                                  user_na_to_na = FALSE,
                                  keep_var_label = FALSE,
                                  user_na_to_tagged_na = FALSE
                                  ) {
  if (!keep_var_label) var_label(x) <- NULL
  val_labels(x) <- NULL
  attr(x, "format.spss") <- NULL
  x
}

#' @export
remove_labels.haven_labelled_spss <- function(x,
                                              user_na_to_na = FALSE,
                                              keep_var_label = FALSE,
                                              user_na_to_tagged_na = FALSE
                                              ) {
  x <- remove_user_na(
    x,
    user_na_to_na = user_na_to_na,
    user_na_to_tagged_na = user_na_to_tagged_na
  )
  if (!keep_var_label) var_label(x) <- NULL
  val_labels(x) <- NULL
  attr(x, "format.spss") <- NULL
  x
}


#' @export
remove_labels.data.frame <- function(x,
                                     user_na_to_na = FALSE,
                                     keep_var_label = FALSE,
                                     user_na_to_tagged_na = FALSE
                                     ) {
  x[] <- lapply(
    x, remove_labels,
    user_na_to_na = user_na_to_na,
    keep_var_label = keep_var_label,
    user_na_to_tagged_na = user_na_to_tagged_na
  )
  x
}

#' @rdname remove_labels
#' @export
remove_var_label <- function(x) {
  UseMethod("remove_var_label")
}


#' @export
remove_var_label.default <- function(x) {
  var_label(x) <- NULL
  x
}

#' @export
remove_var_label.data.frame <- function(x) {
  x[] <- lapply(x, remove_var_label)
  x
}

#' @rdname remove_labels
#' @export
remove_val_labels <- function(x) {
  UseMethod("remove_val_labels")
}


#' @export
remove_val_labels.default <- function(x) {
  val_labels(x) <- NULL
  x
}

#' @export
remove_val_labels.data.frame <- function(x) {
  x[] <- lapply(x, remove_val_labels)
  x
}

#' @rdname remove_labels
#' @export
remove_user_na <- function(x,
                           user_na_to_na = FALSE,
                           user_na_to_tagged_na = FALSE
                           ) {
  UseMethod("remove_user_na")
}


#' @export
remove_user_na.default <- function(x,
                                   user_na_to_na = FALSE,
                                   user_na_to_tagged_na = FALSE
                                   ) {
  # do nothing
  x
}

#' @export
remove_user_na.haven_labelled_spss <- function(x,
                                               user_na_to_na = FALSE,
                                               user_na_to_tagged_na = FALSE
                                               ) {
  if (user_na_to_tagged_na) {
    if (typeof(x) == "character")
      stop("'user_na_to_tagged_na' cannot be used with character labelled vectors.")

    val_to_tag <- x[is.na(x) & !is.na(unclass(x))] %>% unclass()

    if (length(val_to_tag) > 26) {
      warning(
        length(val_to_tag), " different user-defined missing values found in 'x'.\n",
        "A maximum of 26 could be tagged.\n",
        "'user_na_to_tagged_na' has been ignored.\n",
        "'user_na_to_na = TRUE' has been used instead."
      )
      user_na_to_na <- TRUE
    } else {
      if (is.integer(x)) {
        x <- as.double(unclass(x)) %>% copy_labels_from(x)
        message("'x' has been converted into a double vector.")
        val_to_tag <- as.double(val_to_tag)
      }

      user_na_to_na <- FALSE
      vl <- val_labels(x)
      x <- remove_user_na(x) # to avoid error when combining labelled_spss
      for (i in 1:length(val_to_tag)) {
        x[x == val_to_tag[i]] <- tagged_na(letters[i])
        if (val_to_tag[i] %in% vl) {
          vl[vl == val_to_tag[i]] <- tagged_na(letters[i])
        }
      }
      val_labels(x) <- vl
    }
  }

  if (user_na_to_na) {
    x[is.na(x)] <- NA
  }
  na_values(x) <- NULL
  na_range(x) <- NULL
  x
}

#' @export
remove_user_na.data.frame <- function(x,
                                      user_na_to_na = FALSE,
                                      user_na_to_tagged_na = FALSE
                                      ) {
  x[] <- lapply(
    x,
    remove_user_na,
    user_na_to_na = user_na_to_na,
    user_na_to_tagged_na = user_na_to_tagged_na
  )
  x
}
