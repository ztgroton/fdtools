
#' S3 Constructor for Class 'attr_set'
#'
#' @param attr character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- new_attr_set(x)
#' }
new_attr_set <- function(attr) {

  # Validate Input
  if (missing(attr)) {stop("`attr` is missing in call to `new_attr_set`")}

  # Initialize Empty S3 Object
  rs <- new.env()

  # Store `attr` in `rs`
  rs$attr <- attr

  # Set Class
  class(rs) <- c(setdiff('attr_set', class(rs)), class(rs))

  # Return S3 Object
  return(rs)

}

#' S3 Validator for Class 'attr_set'
#'
#' @param obj S3 Object
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate_attr_set(obj, FALSE)
#' }
validate_attr_set <- function(obj, bool) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate_attr_set`")}
  if (missing(bool)) {bool <- FALSE}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * `obj`
  if (!isTRUE(inherits(obj, 'attr_set'))) {
    msg <- "`obj` must inherit from 'attr_set'"
    err <- c(msg, err)
  }

  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }

  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)

  # * `obj`
  if (!isTRUE(is.environment(obj)) || !isTRUE(identical(names(obj), 'attr'))) {
    msg <- "`obj` is not a validly formatted environment in call to `validate_attr_set`"
    err <- c(msg, err)
  }

  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}

  # * `obj$attr`
  is_char <- isTRUE(is.character(obj$attr))
  is_non_na <- !isTRUE(any(purrr::map_lgl(obj$attr, function(x){isTRUE(is.null(x)) || isTRUE(is.na(x))})))
  is_unique <- isTRUE(identical(sort(obj$attr), sort(unique(obj$attr))))

  if (!isTRUE(is_char)) {
    msg <- "`obj$attr` must be type character in call to `validate_attr_set`"
    err <- c(msg, err)
  }

  if (!isTRUE(is_non_na)) {
    msg <- "`obj$attr` must not contain any NULL or NA values in call to `validate_attr_set`"
    err <- c(msg, err)
  }

  if (!isTRUE(is_unique)) {
    msg <- "`obj$attr` must only contain distinct values in call to `validate_attr_set`"
    err <- c(msg, err)
  }

  # Final Output
  if (!isTRUE(bool)) {

      if (isTRUE(length(err) == 0)) {
          return(obj)
      } else {
          return(err)
      }

  } else {

      if (isTRUE(length(err) == 0)) {
          return(TRUE)
      } else {
          return(FALSE)
      }

  }

}

#' S3 Helper Function for Class 'attr_set'
#'
#' @param attr character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- attr_set(x)
#' }
attr_set <- function(attr) {

  # Validate Input
  if (missing(attr)) {stop("`attr` is missing in call to `attr_set`")}

  validate_attr_set(new_attr_set(attr))

}
