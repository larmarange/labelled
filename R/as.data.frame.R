#' as.data.frame method for labelled vectors
#'
#' @param x a labelled vector
#' @export
as.data.frame.labelled <- function(x) {
  if(typeof(x) == "character")
    as.data.frame.character(x, stringsAsFactors = FALSE)
  else
    as.data.frame.numeric(x)
}
