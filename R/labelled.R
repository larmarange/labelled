#' Create a labelled vector.
#'
#' A labelled vector is a common data structure in other statistical environments,
#' allowing you to assign text labels to specific values.
#'
#' @inheritParams haven::labelled
#' @importFrom haven labelled
#' @importFrom stats na.omit
#' @seealso \code{\link[haven]{labelled}} (\pkg{haven})
#' @export
#' @examples
#' s1 <- labelled(c('M', 'M', 'F'), c(Male = 'M', Female = 'F'))
#' s1
#' s2 <- labelled(c(1, 1, 2), c(Male = 1, Female = 2))
#' s2
labelled <- haven::labelled

#' @rdname labelled
#' @inheritParams haven::is.labelled
#' @importFrom haven is.labelled
#' @seealso \code{\link[haven]{is.labelled}} (\pkg{haven})
#' @export
#' @examples
#' is.labelled(s1)
is.labelled <- haven::is.labelled

#' Create a labelled vector with SPSS style of missing values.
#'
#'  It is similar to the \code{\link{labelled}} class but it also models SPSS's
#'  user-defined missings, which can be up to three distinct values,
#'  or for numeric vectors a range.
#'
#' @inheritParams haven::labelled_spss
#' @importFrom haven labelled_spss
#' @seealso \code{\link[haven]{labelled_spss}} (\pkg{haven})
#' @export
#' @examples
#' x1 <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
#' x1
#' is.na(x1)
#' x2 <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_range = c(9, Inf))
#' x2
#' is.na(x2)
labelled_spss <- haven::labelled_spss
