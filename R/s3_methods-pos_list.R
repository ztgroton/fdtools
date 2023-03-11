
#' S3 Generic 'strip_part'
#'
#' @param obj S3 Object
#' @param col character - Vector of column names used to create partition
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' test <- strip_part(pos_list)
#' }
strip_part <- function(obj, col) {UseMethod("strip_part", obj)}

#' S3 Generic 'strip_part' returns a stripped partition
#'
#' @param obj S3 Object
#' @param col character - Vector of column names used to create partition
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' test <- strip_part.pos_list(pos_list)
#' }
strip_part.pos_list <- function(obj, col) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `strip_part.pos_list`")}
  if (missing(col)) {stop("`col` is missing in call to `strip_part_from_record`")}

  # Validate Input Expectations

  # * `obj`
  if (!isTRUE(inherits(obj, 'pos_list'))) {
    stop("`obj` must inherit from 'pos_list' in call to `strip_part.pos_list`")
  }

  # * `col`
  col_in_map <- isTRUE(all(col %in% names(obj$data$pl$map)))
  col_in_index <- isTRUE(all(col %in% names(obj$data$pl$index)))
  col_in_records <- isTRUE(all(col %in% names(obj$data$pl$records)))
  if (!isTRUE(col_in_map) || !isTRUE(col_in_index) || !isTRUE(col_in_records)) {
    stop("`col` must be a subset of columns in `obj` in call to `strip_part_from_record`")
  }

  # Compute Stripped Partition
  result <- strip_part_from_record(pos = obj, col = col)

  # Return Result
  return(result)

}

#' S3 Generic 'key_err'
#'
#' @param obj S3 Object
#' @param col character - Vector of column names used to create partition
#' @param err_only TRUE/FALSE - Optionally specify if only failing values should be returned
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' test <- strip_part(pos_list)
#' }
key_err <- function(obj, col, err_only) {UseMethod("key_err", obj)}

#' S3 Generic 'key_err' returns a stripped partition
#'
#' @param obj S3 Object
#' @param col character - Vector of column names used to create partition
#' @param err_only TRUE/FALSE - Optionally specify if only failing values should be returned
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' test <- strip_part.pos_list(pos_list)
#' }
key_err.pos_list <- function(obj, col, err_only) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `key_err.pos_list`")}
  if (missing(col)) {stop("`col` is missing in call to `key_err.pos_list`")}
  if (missing(err_only)) {err_only <- FALSE}

  # Call `err_key_from_records`
  err_key_from_records(pos = obj, col = col, err_only = err_only)

}

#' S3 Generic 'fd_err'
#'
#' @param obj S3 Object
#' @param xcol character - Vector of column names used to define LHS of FD
#' @param ycol character - Vector of column names used to define RHS of FD
#' @param err_only TRUE/FALSE - Optionally specify if only failing values should be returned
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' test <- strip_part(pos_list)
#' }
fd_err <- function(obj, xcol, ycol, err_only) {UseMethod("fd_err", obj)}

#' S3 Generic 'key_err' returns a stripped partition
#'
#' @param obj S3 Object
#' @param xcol character - Vector of column names used to define LHS of FD
#' @param ycol character - Vector of column names used to define RHS of FD
#' @param err_only TRUE/FALSE - Optionally specify if only failing values should be returned
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' test <- strip_part.pos_list(pos_list)
#' }
fd_err.pos_list <- function(obj, xcol, ycol, err_only) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `fd_err.pos_list`")}
  if (missing(xcol)) {stop("`xcol` is missing in call to `fd_err.pos_list`")}
  if (missing(ycol)) {stop("`ycol` is missing in call to `fd_err.pos_list`")}
  if (missing(err_only)) {err_only <- FALSE}

  # Call `err_fd_from_records`
  err_fd_from_records(pos = obj, xcol = xcol, ycol = ycol, err_only = err_only)

}
