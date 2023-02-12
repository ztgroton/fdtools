
#' Calculate 'Key Error' from Stripped Product
#'
#' @param part S3 Object
#'
#' @return R Object
#'
#' @examples
#' \dontrun{
#' result <- err_key_from_strip_part(strip_part)
#' }
err_key_from_strip_part <- function(part) {

  # Validate Inputs
  if (missing(part)) {stop("`part` is missing in call to `err_key_from_strip_part`")}

}

#' Calculate 'Key Error' from Position List Records
#'
#' @param pos S3 Object of class 'pos_list'
#' @param col character - Vector of column names
#'
#' @return R Object
#'
#' @examples
#' \dontrun{
#' result <- err_key_from_records(strip_part)
#' }
err_key_from_records <- function(pos, col) {

  # Validate Inputs
  if (missing(pos)) {stop("`pos` is missing in call to `err_key_from_records`")}
  if (missing(col)) {stop("`col` is missing in call to `err_key_from_records`")}

  # Validate Input Expectations

  # * `pos`
  if (!isTRUE(inherits(pos, 'pos_list'))) {
    stop("`pos` must inherit from 'pos_list' in call to `err_key_from_records`")
  }

  # * `col`
  col_in_map <- isTRUE(all(col %in% names(pos$data$pl$map)))
  col_in_index <- isTRUE(all(col %in% names(pos$data$pl$index)))
  col_in_records <- isTRUE(all(col %in% names(pos$data$pl$records)))
  if (!isTRUE(col_in_map) || !isTRUE(col_in_index) || !isTRUE(col_in_records)) {
    stop("`col` must be a subset of columns in `pos` in call to `err_key_from_records`")
  }

}

#' Calculate 'Functional Dependency Error' from Stripped Product
#'
#' @param part S3 Object
#'
#' @return R Object
#'
#' @examples
#' \dontrun{
#' result <- err_fd_from_strip_part(strip_part)
#' }
err_fd_from_strip_part <- function(part) {

  # Validate Inputs
  if (missing(part)) {stop("`part` is missing in call to `err_fd_from_strip_part`")}

}

#' Calculate 'Functional Dependency Error' from Position List Records
#'
#' @param pos S3 Object of class 'pos_list'
#' @param col character - Vector of column names
#'
#' @return R Object
#'
#' @examples
#' \dontrun{
#' result <- err_key_from_records(strip_part)
#' }
err_key_from_records <- function(pos, col) {

  # Validate Inputs
  if (missing(pos)) {stop("`pos` is missing in call to `err_key_from_records`")}
  if (missing(col)) {stop("`col` is missing in call to `err_key_from_records`")}

  # Validate Input Expectations

  # * `pos`
  if (!isTRUE(inherits(pos, 'pos_list'))) {
    stop("`pos` must inherit from 'pos_list' in call to `err_key_from_records`")
  }

  # * `col`
  col_in_map <- isTRUE(all(col %in% names(pos$data$pl$map)))
  col_in_index <- isTRUE(all(col %in% names(pos$data$pl$index)))
  col_in_records <- isTRUE(all(col %in% names(pos$data$pl$records)))
  if (!isTRUE(col_in_map) || !isTRUE(col_in_index) || !isTRUE(col_in_records)) {
    stop("`col` must be a subset of columns in `pos` in call to `err_key_from_records`")
  }

}
