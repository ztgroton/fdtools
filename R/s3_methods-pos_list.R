
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
