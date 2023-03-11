
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
  err <- sum(size_sum) - length(part)
  res <- list(err = err, data = part)

  return(res)

}

#' Calculate 'Key Error' from Position List Records
#'
#' @param pos S3 Object of class 'pos_list'
#' @param col character - Vector of column names
#' @param err_only TRUE/FALSE - Optionally specify if only failing values should be returned
#'
#' @return R Object
#'
#' @examples
#' \dontrun{
#' result <- err_key_from_records(pos_list, col_list)
#' }
err_key_from_records <- function(pos, col, err_only) {

  # Validate Inputs
  if (missing(pos)) {stop("`pos` is missing in call to `err_key_from_records`")}
  if (missing(col)) {stop("`col` is missing in call to `err_key_from_records`")}
  if (missing(err_only)) {err_only <- FALSE}

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

  # * `err_only`
  if (!isTRUE(identical(err_only, TRUE)) && !isTRUE(identical(err_only, FALSE))) {
    stop("`err_only` must be TRUE/FALSE in call to `err_key_from_records`")
  }

  # Nest Row Numbers using Distinct Values from 'col'
  data <- pos$data$pl$records %>%
    dplyr::select(dplyr::all_of(col)) %>%
    dplyr::mutate(row_num = dplyr::row_number()) %>%
    tidyr::nest(orig_row_num = c(.data$row_num))

  # Count Nested Rows
  data <- data %>% dplyr::mutate(orig_row_cnt = purrr::map_dbl(.data$orig_row_num, function(x){nrow(x)}))

  # Conditionally Filter Out Nested Row Groups with only one record
  if (isTRUE(err_only)) {
    data <- data %>% dplyr::filter(.data$orig_row_cnt > 1)
  }

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
#' result <- err_fd_from_strip_part(x_part, xy_part)
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
#' @param xcol character - Vector of column names
#' @param ycol character - Vector of column names
#' @param err_only TRUE/FALSE - Optionally specify if only failing values should be returned
#'
#' @return R Object
#'
#' @examples
#' \dontrun{
#' result <- err_key_from_records(pos_list, xcol, ycol)
#' }
err_fd_from_records <- function(pos, xcol, ycol, err_only) {

  # Validate Inputs
  if (missing(pos)) {stop("`pos` is missing in call to `err_fd_from_records`")}
  if (missing(xcol)) {stop("`xcol` is missing in call to `err_fd_from_records`")}
  if (missing(ycol)) {stop("`ycol` is missing in call to `err_fd_from_records`")}
  if (missing(err_only)) {err_only <- FALSE}

  # Validate Input Expectations

  # * `pos`
  if (!isTRUE(inherits(pos, 'pos_list'))) {
    stop("`pos` must inherit from 'pos_list' in call to `err_fd_from_records`")
  }

  # * `xcol`
  xcol_in_map <- isTRUE(all(xcol %in% names(pos$data$pl$map)))
  xcol_in_index <- isTRUE(all(xcol %in% names(pos$data$pl$index)))
  xcol_in_records <- isTRUE(all(xcol %in% names(pos$data$pl$records)))
  if (!isTRUE(xcol_in_map) || !isTRUE(xcol_in_index) || !isTRUE(xcol_in_records)) {
    stop("`xcol` must be a subset of columns in `pos` in call to `err_fd_from_records`")
  }

  # * `ycol`
  ycol_in_map <- isTRUE(all(ycol %in% names(pos$data$pl$map)))
  ycol_in_index <- isTRUE(all(ycol %in% names(pos$data$pl$index)))
  ycol_in_records <- isTRUE(all(ycol %in% names(pos$data$pl$records)))
  if (!isTRUE(ycol_in_map) || !isTRUE(ycol_in_index) || !isTRUE(ycol_in_records)) {
    stop("`ycol` must be a subset of columns in `pos` in call to `err_fd_from_records`")
  }

  # * `xcol` & `ycol`
  if (!isTRUE(length(intersect(xcol, ycol)) == 0)) {
    stop("`xcol` and `ycol` must be disjoint in call to `err_fd_from_records`")
  }

  # * `err_only`
  if (!isTRUE(identical(err_only, TRUE)) && !isTRUE(identical(err_only, FALSE))) {
    stop("`err_only` must be TRUE/FALSE in call to `err_fd_from_records`")
  }

  # Nest Row Numbers using Distinct Values from `union(xcol,ycol)`
  data <- pos$data$pl$records %>%
    dplyr::select(dplyr::all_of(union(xcol,ycol))) %>%
    dplyr::mutate(xy_row_num = dplyr::row_number()) %>%
    tidyr::nest(orig_xy_row_num = c(.data$xy_row_num))

  # Count Nested Rows
  data <- data %>% dplyr::mutate(orig_xy_row_cnt = purrr::map_dbl(.data$orig_xy_row_num, function(x){nrow(x)}))

  # Nest Columns in `data` using Distinct Values from `xcol`
  data <- data %>% tidyr::nest(.by = dplyr::all_of(xcol), .key = 'yval')

  # Count Nested Rows
  data <- data %>% dplyr::mutate(yval_cnt = purrr::map_dbl(.data$yval, function(x){nrow(x)}))

  # Conditionally Filter Out Nested Row Groups with only one record
  if (isTRUE(err_only)) {
    data <- data %>% dplyr::filter(.data$yval_cnt > 1)
  }

  return(data)

}
