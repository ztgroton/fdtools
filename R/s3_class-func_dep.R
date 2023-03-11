
#' S3 Constructor for Class 'func_dep'
#'
#' @param lhs S3 Object - attr_set
#' @param rhs S3 Object - attr_set
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- new_func_dep(lhs, rhs)
#' }
new_func_dep <- function(lhs, rhs) {

  # Initialize Empty S3 Object
  rs <- list(
    data = new.env()
  )

  # Store `lhs`/`rhs` in `rs$data`
  rs$data$lhs <- lhs
  rs$data$rhs <- rhs

  # Set Class
  class(rs) <- c(setdiff('func_dep', class(rs)), class(rs))

  # Return S3 Object
  return(rs)

}

#' S3 Validator for Class 'func_dep'
#'
#' @param obj S3 Object
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- validate_func_dep(obj, TRUE)
#' }
validate_func_dep <- function(obj, bool) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate_func_dep`")}
  if (missing(bool)) {bool <- FALSE}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * `obj`
  if (!isTRUE(inherits(obj, 'func_dep'))) {
    msg <- "`obj` must inherit from 'func_dep'"
    err <- c(msg, err)
  }

  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }

  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)

  # * `obj$data`
  if (!isTRUE(is.environment(obj$data)) || !isTRUE(identical(sort(names(obj$data)), sort(c('lhs', 'rhs'))))) {
    msg <- "`obj$data` is not a validly formatted environment in call to `validate_func_dep`"
    err <- c(msg, err)
  }

  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}

  # * `obj$data$lhs`
  if (!isTRUE(validate_attr_set(obj$data$lhs, TRUE))) {
    msg <- "`obj$data$lhs` is not a valid 'attr_set' in call to `validate_func_dep`"
    err <- c(msg, err)
  }

  # * `obj$data$rhs`
  if (!isTRUE(validate_attr_set(obj$data$rhs, TRUE))) {
    msg <- "`obj$data$rhs` is not a valid 'attr_set' in call to `validate_func_dep`"
    err <- c(msg, err)
  }

  # * `obj$data$lhs` / `obj$data$rhs`
  if (!isTRUE(length(intersect(obj$data$lhs$attributes, obj$data$rhs$attributes)) == 0)) {
    msg <- "`obj$data$lhs` must not overlap with `obj$data$rhs` in call to `validate_func_dep`"
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

#' S3 Helper Function for Class 'func_dep'
#'
#' @param lhs S3 Object
#' @param rhs S3 Object
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- func_dep(lhs, rhs)
#' }
func_dep <- function(lhs, rhs) {

  # Validate Input
  if (missing(lhs)) {stop("`lhs` is missing in call to `func_dep`")}
  if (missing(rhs)) {stop("`rhs` is missing in call to `func_dep`")}

  validate_func_dep(new_func_dep(lhs, rhs))

}
