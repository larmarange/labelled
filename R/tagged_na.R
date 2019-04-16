#' "Tagged" missing values
#'
#' "Tagged" missing values work exactly like regular R missing values except
#' that they store one additional byte of information a tag, which is usually
#' a letter ("a" to "z"). When by loading a SAS and Stata file, the tagged
#' missing values always use lower case values.
#'
#' `format_tagged_na()` and `print_tagged_na()` format tagged
#' NA's as NA(a), NA(b), etc.
#'
#' @inheritParams haven::tagged_na
#' @importFrom haven tagged_na
#' @export
#' @examples
#' x <- c(1:5, tagged_na("a"), tagged_na("z"), NA)
#'
#' # Tagged NA's work identically to regular NAs
#' x
#' is.na(x)
#'
#' # To see that they're special, you need to use na_tag(),
#' # is_tagged_na(), or print_tagged_na():
#' is_tagged_na(x)
#' na_tag(x)
#' print_tagged_na(x)
#'
#' # You can test for specific tagged NAs with the second argument
#' is_tagged_na(x, "a")
#'
#' # Because the support for tagged's NAs is somewhat tagged on to R,
#' # the left-most NA will tend to be preserved in arithmetic operations.
#' na_tag(tagged_na("a") + tagged_na("z"))
tagged_na <- haven::tagged_na

#' @rdname tagged_na
#' @inheritParams haven::na_tag
#' @importFrom haven na_tag
#' @export
na_tag <- haven::na_tag

#' @rdname tagged_na
#' @inheritParams haven::is_tagged_na
#' @importFrom haven is_tagged_na
#' @export
is_tagged_na <- haven::is_tagged_na

#' @rdname tagged_na
#' @inheritParams haven::format_tagged_na
#' @importFrom haven format_tagged_na
#' @export
format_tagged_na <- haven::format_tagged_na

#' @rdname tagged_na
#' @inheritParams haven::print_tagged_na
#' @importFrom haven print_tagged_na
#' @export
print_tagged_na <- haven::print_tagged_na
