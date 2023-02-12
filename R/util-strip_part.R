
#' Generate 'Stripped Partition' from Position List Records
#'
#' @importFrom rlang .data
#'
#' @param pos S3 Object of class 'pos_list'
#' @param col character - Vector of column names used to create partition
#'
#' @return S3 Object
#'
#' @examples
#' \dontrun{
#' output <- strip_part_from_record(pos_list, column_names)
#' }
strip_part_from_record <- function(pos, col) {

  # Validate Inputs
  if (missing(pos)) {stop("`pos` is missing in call to `strip_part_from_record`")}
  if (missing(col)) {stop("`col` is missing in call to `strip_part_from_record`")}

  # Validate Input Expectations

  # * `pos`
  if (!isTRUE(inherits(pos, 'pos_list'))) {
    stop("`pos` must inherit from 'pos_list' in call to `strip_part_from_record`")
  }

  # * `col`
  col_in_map <- isTRUE(all(col %in% names(pos$data$pl$map)))
  col_in_index <- isTRUE(all(col %in% names(pos$data$pl$index)))
  col_in_records <- isTRUE(all(col %in% names(pos$data$pl$records)))
  if (!isTRUE(col_in_map) || !isTRUE(col_in_index) || !isTRUE(col_in_records)) {
    stop("`col` must be a subset of columns in `pos` in call to `strip_part_from_record`")
  }

  # Nest Row Numbers using Distinct Values from 'col'
  data <- pos$data$pl$records %>%
    dplyr::select(dplyr::all_of(col)) %>%
    dplyr::mutate(row_num = dplyr::row_number()) %>%
    tidyr::nest(orig_row_num = c(.data$row_num))

  # Count Nested Rows
  data <- data %>% dplyr::mutate(orig_row_cnt = purrr::map_dbl(.data$orig_row_num, function(x){nrow(x)}))

  # Drop 'col' Columns
  data <- data %>% dplyr::select(.data$orig_row_num, .data$orig_row_cnt)

  # Create New 'row_number' Column
  data <- data %>% dplyr::mutate(val_row_num = dplyr::row_number())

  # Filter Out Nested Row Groups with only one record
  data <- data %>%
    dplyr::filter(.data$orig_row_cnt > 1) %>%
    dplyr::select(-.data$orig_row_cnt)

  # Unnest 'orig_row_num'
  data <- data %>% tidyr::unnest(.data$orig_row_num)

  # Split 'row_num' by 'val_row_num'
  res <- split(data$row_num, factor(data$val_row_num, exclude = NULL))

  return(res)

}

#' Generate 'Stripped Partition' from existing Stripped Partitions
#'
#' @param lhs Stripped Partition
#' @param rhs Stripped Partition
#'
#' @return Stripped Partition
#'
#' @examples
#' \dontrun{
#' xy <- strip_product_r(x,y)
#' }
strip_product_r <- function(lhs, rhs) {

  # Validate Inputs
  if (missing(lhs)) {stop("`lhs` is missing in call to `strip_product_r`")}
  if (missing(rhs)) {stop("`rhs` is missing in call to `strip_product_r`")}

  # Validate Input Expectations
  if (!isTRUE(is.list(lhs)) || !isTRUE(length(lhs) > 0)) {
    stop("`lhs` must be a non-empty list in call to `strip_product_r`")
  }
  if (!isTRUE(is.list(rhs)) || !isTRUE(length(rhs) > 0)) {
    stop("`rhs` must be a non-empty list in call to `strip_product_r`")
  }

  # Enframe `lhs`
  lhs <- tibble::enframe(lhs) %>%
    tidyr::unnest(.data$value) %>%
    dplyr::rename(lhs_name = .data$name)

  # Enframe `rhs`
  rhs <- tibble::enframe(rhs) %>%
    tidyr::unnest(.data$value) %>%
    dplyr::rename(rhs_name = .data$name)

  # Inner Join 'lhs' and 'rhs' on 'value'
  res <- lhs %>% dplyr::inner_join(rhs, by = 'value')

  # Nest Row Numbers / Compute 'value_row_num'
  res <- res %>%
    dplyr::rename(row_num = .data$value) %>%
    tidyr::nest(orig_row_num = c(.data$row_num)) %>%
    dplyr::mutate(orig_row_cnt = purrr::map_dbl(.data$orig_row_num, function(x){nrow(x)})) %>%
    dplyr::mutate(val_row_num = dplyr::row_number())

  # Filter out records where 'orig_row_num' has only one entry
  res <- res %>% dplyr::filter(.data$orig_row_cnt > 1)

  # Unnest 'orig_row_num'
  data <- data %>%
    dplyr::select(.data$val_row_num, .data$orig_row_num) %>%
    tidyr::unnest(.data$orig_row_num)

  # Split 'row_num' by 'val_row_num'
  res <- split(data$row_num, factor(data$val_row_num, exclude = NULL))

  # Return
  return(res)

}

#' Test Stripped Partitions for Equality
#'
#' @param x Stripped Partition
#' @param y Stripped Partition
#'
#' @return R Object
#'
#' @examples
#' \dontrun{
#' test <- strip_part_equal(x,y)
#' }
strip_part_equal <- function(x, y) {

  # Validate Inputs
  if (missing(x)) {stop("`x` is missing in call to `strip_product_r`")}
  if (missing(y)) {stop("`y` is missing in call to `strip_product_r`")}

  # Validate Input Expectations
  if (!isTRUE(is.list(x)) || !isTRUE(length(x) > 0)) {
    stop("`x` must be a non-empty list in call to `strip_product_r`")
  }
  if (!isTRUE(is.list(y)) || !isTRUE(length(y) > 0)) {
    stop("`y` must be a non-empty list in call to `strip_product_r`")
  }

  # Calculate Intersection
  x_y <- intersect(x,y)

  # Return Result
  res <- isTRUE(length(x_y) == length(x))
  res <- res && isTRUE(length(x_y) == length(y))
  return(res)

}
