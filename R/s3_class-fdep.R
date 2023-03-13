
#' S3 Constructor for Class 'fdep'
#'
#' @param lhs S3 Object - attr_set
#' @param rhs S3 Object - attr_set
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- new_fdep(lhs, rhs)
#' }
new_fdep <- function(lhs, rhs) {

  # Initialize Empty S3 Object
  rs <- new.env()

  # Store `lhs`/`rhs` in `rs`
  rs$lhs <- lhs
  rs$rhs <- rhs

  # Set Class
  class(rs) <- c(setdiff('fdep', class(rs)), class(rs))

  # Return S3 Object
  return(rs)

}

#' S3 Validator for Class 'fdep'
#'
#' @param obj S3 Object
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- validate_fdep(obj, TRUE)
#' }
validate_fdep <- function(obj, bool) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate_fdep`")}
  if (missing(bool)) {bool <- FALSE}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * `obj`
  if (!isTRUE(inherits(obj, 'fdep'))) {
    msg <- "`obj` must inherit from 'fdep'"
    err <- c(msg, err)
  }

  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }

  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)

  # * `obj`
  if (!isTRUE(is.environment(obj)) || !isTRUE(identical(sort(names(obj)), sort(c('lhs', 'rhs'))))) {
    msg <- "`obj` is not a validly formatted environment in call to `validate_fdep`"
    err <- c(msg, err)
  }

  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}

  # * `obj$lhs`
  if (!isTRUE(validate_attr_set(obj$lhs, TRUE))) {
    msg <- "`obj$lhs` is not a valid 'attr_set' in call to `validate_fdep`"
    err <- c(msg, err)
  }

  # * `obj$rhs`
  if (!isTRUE(validate_attr_set(obj$rhs, TRUE))) {
    msg <- "`obj$rhs` is not a valid 'attr_set' in call to `validate_fdep`"
    err <- c(msg, err)
  }

  # * `obj$lhs` / `obj$rhs`
  if (!isTRUE(length(intersect(obj$lhs$attr, obj$rhs$attr)) == 0)) {
    msg <- "`obj$lhs` must not overlap with `obj$rhs` in call to `validate_fdep`"
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

#' S3 Helper Function for Class 'fdep'
#'
#' @param lhs S3 Object
#' @param rhs S3 Object
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- fdep(lhs, rhs)
#' }
fdep <- function(lhs, rhs) {

  # Validate Input
  if (missing(lhs)) {stop("`lhs` is missing in call to `fdep`")}
  if (missing(rhs)) {stop("`rhs` is missing in call to `fdep`")}

  validate_fdep(new_fdep(lhs, rhs))

}
