
#' S3 Constructor for Class 'pos_list'
#'
#' @param frame data.frame
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- new_pos_list(raw_data_frame)
#' }
new_pos_list <- function(frame) {

  # Validate Inputs
  if (missing(frame)) {stop("`frame` is missing in call to `new_pos_list`")}

  # Initialize Empty S3 Object
  rs <- list(data = new.env())

  # Convert 'frame' to Position List
  rs$data$pl <- frame_to_pl(frame)

  # Set Class
  class(rs) <- c(setdiff('pos_list', class(rs)), class(rs))

  # Return S3 Object
  return(rs)

}


#' S3 Validator for Class 'pos_list'
#'
#' @param obj S3 Object
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate_pos_list(s3_obj, FALSE)
#' }
validate_pos_list <- function(obj, bool) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate_pos_list`")}
  if (missing(bool)) {bool <- FALSE}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * `obj`
  if (!isTRUE(inherits(obj, 'pos_list'))) {
    msg <- "`obj` must inherit from 'pos_list'"
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


#' S3 Helper Function for Class 'pos_list'
#'
#' @param frame data.frame
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- pos_list(IMF_DATA)
#' }
pos_list <- function(frame) {

  # Validate Inputs
  if (missing(frame)) {stop("`frame` is missing in call to `pos_list`")}

  validate_pos_list(new_pos_list(frame))

}
