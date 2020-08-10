#' Look for keywords variable names and descriptions
#'
#' `look_for` emulates the `lookfor` Stata command in \R. It supports
#' searching into the variable names of regular \R data frames as well as into
#' variable labels descriptions.
#' The command is meant to help users finding variables in large datasets.
#'
#' @param data a data frame
#' @param ... list of keywords, a character string (or several character strings), which can be
#' formatted as a regular expression suitable for a [base::grep()] pattern, or a vector of keywords;
#' displays all variables if not specified
#' @param labels whether or not to search variable labels (descriptions); `TRUE` by default
#' @param ignore.case whether or not to make the keywords case sensitive;
#' `TRUE` by default (case is ignored during matching)
#' @param details add details about each variable (see examples)
#' @return a tibble data frame featuring the variable position, name and description
#' (if it exists) in the original data frame
#' @details The function looks into the variable names for matches to the keywords. If available,
#' variable labels are included in the search scope.
#' Variable labels of data.frame imported with \pkg{foreign} or
#' \pkg{memisc} packages will also be taken into account (see [to_labelled()]).
#'
#' `look_for()` and `lookfor()` are equivalent.
#' @author François Briatte <f.briatte@@gmail.com>, Joseph Larmarange <joseph@@larmarange.net>
#' @importFrom dplyr tibble
#' @importFrom vctrs vec_ptype_abbr
#' @examples
#' look_for(iris)
#' # Look for a single keyword.
#' look_for(iris, "petal")
#' look_for(iris, "s")
#' # Look for with a regular expression
#' look_for(iris, "petal|species")
#' look_for(iris, "s$")
#' # Look for with several keywords
#' look_for(iris, "pet", "sp")
#' look_for(iris, "pet", "sp", "width")
#' # Labelled data
#' \dontrun{
#' data(fertility, package = "questionr")
#' look_for(women)
#' look_for(women, "date")
#' # Display details
#' look_for(women, details = TRUE)
#' }
#' @source Based on the behaviour of the `lookfor` command in Stata.
#' @export

look_for <- function(data,
                    ...,
                    labels = TRUE,
                    ignore.case = TRUE,
                    details = FALSE) {
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
  # get col_types
  col_types <- unlist(lapply(data, vctrs::vec_ptype_abbr))

  # output
  if(length(variable)) {
    pos <- which(n %in% variable)
    # reordering according to pos
    # not forgetting that some variables don't have a label
    if (length(l)) {
      res <- dplyr::tibble(pos = pos, variable = n[pos], label = l[n[pos]], col_type = col_types[pos])
    } else {
      res <- dplyr::tibble(pos = pos, variable = n[pos], label = NA_character_, col_type = col_types[pos])
    }

    if (details) {
      res$class <- NA_character_
      res$type <- NA_character_
      res$levels <- NA_character_
      res$value_labels <- NA_character_
      res$unique_values <- NA_integer_
      res$n_na <- NA_integer_
      res$na_values <- NA_character_
      res$na_range <- NA_character_
      for (i in 1:nrow(res)) {
        v <- res$variable[i]
        res$class[i] <- paste(class(data[[v]]), collapse = ", ")
        res$type[i] <- paste(typeof(data[[v]]), collapse = ", ")
        if (is.factor(data[[v]]))
          res$levels[i] <- paste(levels(data[[v]]), collapse = "; ")
        if (inherits(data[[v]], "haven_labelled")) {
          res$value_labels[i] <- paste(names(val_labels(data[[v]], prefixed=TRUE)), collapse = "; ")
          res$na_values[i] <- paste(na_values(data[[v]]), collapse = ", ")
          res$na_range[i] <- paste(na_range(data[[v]]), collapse = "-")
        }
        res$unique_values[i] <- length(unique(data[[v]]))
        res$n_na[i] <- sum(is.na(data[[v]]))
      }
    }
  } else {
    res <- dplyr::tibble()
  }
  # add a look_for class
  class(res) <- c("look_for", class(res))
  attr(res, "details") <- details
  res
}


#' @rdname look_for
#' @export

lookfor <- function(data,
                     ...,
                     labels = TRUE,
                     ignore.case = TRUE,
                     details = FALSE) {
  look_for(data = data, ..., labels = labels, ignore.case = ignore.case, details = details)
}

#' @rdname look_for
#' @importFrom pillar colonnade
#' @export
print.look_for <- function(x, ...) {
  if (nrow(x) > 0) {
    r <- x
    r$label[is.na(r$label)] <- "\u2014" # display -- when empty
    print(pillar::colonnade(r, has_row_id = FALSE))
  } else {
    message("Nothing found. Sorry.")
  }
}
