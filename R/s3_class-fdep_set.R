
new_fdep_set <- function() {

  # Initialize Empty S3 Object
  rs <- new.env()

  # Initialize `rs$fd`
  rs$fd <-

  # Set Class
  class(rs) <- c(setdiff('fdep_set', class(rs)), class(rs))

  # Return S3 Object
  return(rs)

}

validate_fdep_set <- function(obj, bool) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate_fdep_set`")}
  if (missing(bool)) {bool <- FALSE}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * `obj`
  if (!isTRUE(inherits(obj, 'fdep_set'))) {
    msg <- "`obj` must inherit from 'fdep_set'"
    err <- c(msg, err)
  }

  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }

  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}

  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)

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

fdep_set <- function() {

  validate_fdep_set(new_fdep_set())

}
