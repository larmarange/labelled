## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom dplyr .data
## usethis namespace: end
NULL

# because `where` is not exported by tidyselect
# cf. https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables("where")

