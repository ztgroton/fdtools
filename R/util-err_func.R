
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

  # Validate Input Expectations
  if (!isTRUE(is.list(part)) || !isTRUE(length(part) > 0)) {
    stop("`part` must be a non-empty list in call to `err_key_from_strip_part`")
  }

  # Calculate Sum of the Size of Equivalence Classes in 'part'
  size_sum <- purrr::map_dbl(part, function(x) {length(x)})

  # Return Result
  err <- size_sum - length(part)
  res <- list(err = err, data = part)
  return(res)

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
#' result <- err_key_from_records(pos_list, col_list)
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

  # Nest Row Numbers using Distinct Values from 'col'
  data <- pos$data$pl$records %>%
    dplyr::select(dplyr::all_of(col)) %>%
    dplyr::mutate(row_num = dplyr::row_number()) %>%
    tidyr::nest(orig_row_num = c(.data$row_num))

  # Count Nested Rows
  data <- data %>% dplyr::mutate(orig_row_cnt = purrr::map_dbl(.data$orig_row_num, function(x){nrow(x)}))

  # Filter Out Nested Row Groups with only one record
  data <- data %>% dplyr::filter(.data$orig_row_cnt > 1)

  # Calculate Sum of the Size of Equivalence Classes in 'part'
  size_sum <- sum(data$orig_row_cnt)

  # Return Result
  err <- size_sum - nrow(data)
  res <- list(err = err, data = data)
  return(res)

}

#' Calculate 'Functional Dependency Error' from Stripped Product
#'
#' @param x_part S3 Object
#' @param xy_part S3 Object
#'
#' @return R Object
#'
#' @examples
#' \dontrun{
#' result <- err_fd_from_strip_part(strip_part)
#' }
err_fd_from_strip_part <- function(x_part, xy_part) {

  # Validate Inputs
  if (missing(x_part)) {stop("`x_part` is missing in call to `err_fd_from_strip_part`")}
  if (missing(xy_part)) {stop("`xy_part` is missing in call to `err_fd_from_strip_part`")}

  # Validate Input Expectations
  if (!isTRUE(is.list(x_part)) || !isTRUE(length(x_part) > 0)) {
    stop("`x_part` must be a non-empty list in call to `err_key_from_strip_part`")
  }
  if (!isTRUE(is.list(xy_part)) || !isTRUE(length(xy_part) > 0)) {
    stop("`xy_part` must be a non-empty list in call to `err_key_from_strip_part`")
  }

  #


}

#' Calculate 'Functional Dependency Error' from Position List Records
#'
#' @param pos S3 Object of class 'pos_list'
#' @param x_col character - Vector of column names
#' @param y_col character - Vector of column names
#'
#' @return R Object
#'
#' @examples
#' \dontrun{
#' result <- err_key_from_records(strip_part)
#' }
err_fd_from_records <- function(pos, x_col, y_col) {

  # Validate Inputs
  if (missing(pos)) {stop("`pos` is missing in call to `err_fd_from_records`")}
  if (missing(col)) {stop("`col` is missing in call to `err_fd_from_records`")}

  # Validate Input Expectations

  # * `pos`
  if (!isTRUE(inherits(pos, 'pos_list'))) {
    stop("`pos` must inherit from 'pos_list' in call to `err_fd_from_records`")
  }

  # * `col`
  col_in_map <- isTRUE(all(col %in% names(pos$data$pl$map)))
  col_in_index <- isTRUE(all(col %in% names(pos$data$pl$index)))
  col_in_records <- isTRUE(all(col %in% names(pos$data$pl$records)))
  if (!isTRUE(col_in_map) || !isTRUE(col_in_index) || !isTRUE(col_in_records)) {
    stop("`col` must be a subset of columns in `pos` in call to `err_fd_from_records`")
  }

}
