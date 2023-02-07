
#' S3 Generic 'strip_part'
#'
#' @param obj S3 Object
#' @param ... ellipsis
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' test <- strip_part(pos_list)
#' }
strip_part <- function(obj, ...) {UseMethod("strip_part", obj)}

#' S3 Generic 'strip_part' returns a stripped partition
#'
#' @param obj S3 Object
#' @param ... ellipsis
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' test <- strip_part.pos_list(pos_list)
#' }
strip_part.pos_list <- function(obj, ...) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `strip_part.pos_list`")}

  # Validate Input Expectations
  if (!isTRUE(inherits(obj, 'pos_list'))) {
    stop("`obj` must inherit from 'pos_list' in call to `strip_part.pos_list`")
  }

}
