# Using setattr from data.table if available,
# attr<- otherwise.
# The purpose us to avoid copy of objects in memory
.setattr <- function(x, name, value) {
  if (requireNamespace("data.table", quietly = TRUE))
    data.table::setattr(x, name, value)
  else
    attr(x, name) <- value
  x
}
