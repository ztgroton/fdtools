
#' S3 Generic - Return Character Vector of Attributes
#'
#' @param obj S3 Object
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' result <- get_attr(attr_set)
#' }
get_attr <- function(obj) {UseMethod("get_attr", obj)}

#' S3 Method - Return Character Vector of Attributes
#'
#' @param obj S3 Object
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' result <- get_attr(attr_set)
#' }
get_attr.attr_set <- function(obj) {

  # Validate Inputs

  # Validate Input Expectations

}

#' S3 Generic - Add New Attributes to Attribute Set
#'
#' @param obj S3 Object
#' @param x character
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' add_attr(attr_set, x)
#' }
add_attr <- function(obj, x) {UseMethod("add_attr", obj)}

#' S3 Method - Add New Attributes to Attribute Set
#'
#' @param obj S3 Object
#' @param x character
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' add_attr(attr_set, x)
#' }
add_attr.attr_set <- function(obj, x) {

  # Validate Inputs

  # Validate Input Expectations

}

#' S3 Generic - Remove Attributes from Attribute Set
#'
#' @param obj S3 Object
#' @param x character
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' rm_attr(attr_set, x)
#' }
rm_attr <- function(obj, x) {UseMethod("rm_attr", obj)}

#' S3 Method - Remove Attributes from Attribute Set
#'
#' @param obj S3 Object
#' @param x character
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' rm_attr(attr_set, x)
#' }
rm_attr.attr_set <- function(obj, x) {

  # Validate Inputs

  # Validate Input Expectations

}
