
#' S3 Generic - Return Character Vector of LHS Attributes
#'
#' @param obj S3 Object
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' result <- get_lhs(fdep)
#' }
get_lhs <- function(obj) {UseMethod("get_lhs", obj)}

#' S3 Method - Return Character Vector of LHS Attributes
#'
#' @param obj S3 Object
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' result <- get_lhs(fdep)
#' }
get_lhs.fdep <- function(obj) {

  # Validate Inputs

  # Validate Input Expectations

}

#' S3 Generic - Add New Attributes to LHS Attribute Set
#'
#' @param obj S3 Object
#' @param x character
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' add_lhs(fdep, x)
#' }
add_lhs <- function(obj, x) {UseMethod("add_lhs", obj)}

#' S3 Method - Add New Attributes to LHS Attribute Set
#'
#' @param obj S3 Object
#' @param x character
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' add_lhs(fdep, x)
#' }
add_lhs.fdep <- function(obj, x) {

  # Validate Inputs

  # Validate Input Expectations

}

rm_lhs <- function(obj, x) {UseMethod("rm_lhs", obj)}

rm_lhs.fdep <- function(obj, x) {

  # Validate Inputs

  # Validate Input Expectations

}

#' S3 Generic - Return Character Vector of RHS Attributes
#'
#' @param obj S3 Object
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' result <- get_rhs(fdep)
#' }
get_rhs <- function(obj) {UseMethod("get_rhs", obj)}

#' S3 Method - Return Character Vector of RHS Attributes
#'
#' @param obj S3 Object
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' result <- get_rhs(fdep)
#' }
get_rhs.fdep <- function(obj) {

  # Validate Inputs

  # Validate Input Expectations

}

add_rhs <- function(obj, x) {UseMethod("add_rhs", obj)}

add_rhs.fdep <- function(obj, x) {

  # Validate Inputs

  # Validate Input Expectations

}

rm_rhs <- function(obj, x) {UseMethod("rm_rhs", obj)}

rm_rhs.fdep <- function(obj, x) {

  # Validate Inputs

  # Validate Input Expectations

}
