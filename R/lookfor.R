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
        all(unlist(lapply(toto$levels, is.null)))

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

lookfor <- function(data,
                     ...,
                     labels = TRUE,
                     ignore.case = TRUE,
                     details = TRUE) {
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
