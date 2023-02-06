
#' Convert R DataFrame into a 'Position List'
#'
#' @param data data.frame
#' @param index TRUE/FALSE
#' @param records TRUE/FALSE
#' @param ... ellipsis
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' pos_list <- frame_to_pl(raw_data_frame, TRUE, TRUE)
#' }
frame_to_pl <- function(data, index, records, ...) {

  # Validate Inputs
  if (missing(data)) {stop("`data` is missing in call to `frame_to_pl`")}
  if (missing(index)) {index <- TRUE}
  if (missing(records)) {records <- TRUE}

  # Validate Input Expectations

  # * data
  if (!isTRUE(is.data.frame(data))) {
    stop("`data` must be type 'data.frame' in call to `frame_to_pl`")
  }

  # * index
  if (!isTRUE(identical(index, TRUE)) && !isTRUE(identical(index, FALSE))) {
    stop("`index` must equal TRUE/FALSE in call to `frame_to_pl`")
  }

  # * records
  if (!isTRUE(identical(records, TRUE)) && !isTRUE(identical(records, FALSE))) {
    stop("`records` must equal TRUE/FALSE in call to `frame_to_pl`")
  }

  # Call 'gen_pl_records'
  result <- gen_pl_records(data)

  # Conditionally Remove Components
  if (!isTRUE(index)) {result[['index']] <- NULL}
  if (!isTRUE(records)) {result[['records']] <- NULL}

  # Return
  return(result)

}

#' Convert a 'Position List' into R DataFrame
#'
#' @param pl R Object
#' @param use_index TRUE/FALSE
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' new_df <- pl_to_frame(pl, TRUE)
#' }
pl_to_frame <- function(pl, use_index) {

  # Validate Inputs
  if (missing(pl)) {stop("`pl` is missing in call to `pl_to_frame`")}
  if (missing(use_index)) {use_index <- FALSE}

  # Validate Input Expectations

  # * pl
  expected_pl_names <- c('map', 'index', 'records')
  if (!isTRUE(identical(sort(names(pl)), sort(expected_pl_names)))) {
    stop("`pl` must be list with names `c('map', 'index', 'records')` in call to `pl_to_frame`")
  }

  # * use_index
  if (!isTRUE(identical(use_index, TRUE)) && !isTRUE(identical(use_index, FALSE))) {
    stop("`use_index` must equal TRUE/FALSE in call to `pl_to_frame`")
  }

  # Save 'Map' names for later use
  pl_map_names <- names(pl$map)

  # Conditionally use 'Index' or 'Records'
  if (isTRUE(use_index)) {

    result <- lapply(pl_map_names, function(x) {

      data_x <- pl$index[[x]] %>%
        tibble::enframe() %>%
        tidyr::unnest(.data$value) %>%
        dplyr::arrange(.data$value) %>%
        dplyr::select(-.data$value) %>%
        dplyr::rename(index = .data$name) %>%
        dplyr::mutate(index = as.integer(.data$index))

      data_x <- data_x %>% dplyr::left_join(pl$map[[x]], by = 'index')
      data_x[[x]] <- data_x$val
      data_x$val <- NULL

      data_x

    })

    result <- dplyr::bind_cols(result)

  } else {

    result <- lapply(pl_map_names, function(x) {

      data_x <- pl$records[, c(x), drop = F]
      join_cond <- c('index')
      names(join_cond) <- x

      data_x <- data_x %>%
        dplyr::left_join(pl$map[[x]], by = join_cond) %>%
        dplyr::select(.data$val)

      data_x[[x]] <- data_x$val
      data_x$val <- NULL

      data_x

    })

    result <- dplyr::bind_cols(result)

  }

  # Return
  return(result)

}

#' Generate 'Map' component of a 'Position List'
#'
#' @param data data.frame
#' @param ... ellipsis
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' map <- gen_pl_map(raw_data_frame)
#' }
gen_pl_map <- function(data, ...) {

  # Validate Inputs
  if (missing(data)) {stop("`data` is missing in call to `gen_pl_map`")}

  # Validate Input Expectations

  # * `data`
  if (!isTRUE(is.data.frame(data))) {
    stop("`data` must be type 'data.frame' in call to `gen_pl_map`")
  }

  if (!isTRUE(nrow(data) > 0)) {
    stop("`data` must have non-zero row count in call to `gen_pl_map`")
  }

  # Save 'colnames(data)' for later use
  data_colnames <- colnames(data)

  # Convert 'data' into named list of distinct values for each column
  data <- lapply(data, function(x) {
    unq_val <- unique(x)
    data.frame(
      val = unq_val,
      index = 1:length(unq_val),
      stringsAsFactors = FALSE
    )
  })
  names(data) <- data_colnames

  # Return
  return(data)

}

#' Generate 'Index' component of a 'Position List'
#'
#' @importFrom rlang .data
#'
#' @param data data.frame
#' @param ... ellipsis
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' map <- gen_pl_map(raw_data_frame)
#' }
gen_pl_index <- function(data, ...) {

  # Validate Inputs
  if (missing(data)) {stop("`data` is missing in call to `gen_pl_index`")}

  # Validate Input Expectations

  # * `data`
  if (!isTRUE(is.data.frame(data))) {
    stop("`data` must be type 'data.frame' in call to `gen_pl_index`")
  }

  if (!isTRUE(nrow(data) > 0)) {
    stop("`data` must have non-zero row count in call to `gen_pl_index`")
  }

  # Save 'colnames(data)' for later use
  data_colnames <- colnames(data)

  # * Generate 'Map'
  sink("NUL")
  data_map <- gen_pl_map(data)
  sink()

  # Generate 'Index'
  data_index <- lapply(data_colnames, function(x) {

    # * Process Individual Columns
    cat(paste0("'", x, "'..."))
    tictoc::tic()

    data_x <- data %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::mutate(row_num = dplyr::row_number())

    join_cond <- c('val')
    names(join_cond) <- x

    data_x <- data_x %>% dplyr::left_join(data_map[[x]], by = join_cond)

    res <- split(data_x$row_num, factor(data_x$index, exclude = NULL))

    tictoc::toc()
    return(res)

  })
  names(data_index) <- data_colnames

  # Save Results
  result <- list(map = data_map, index = data_index)

  # Return Results
  return(result)

}

#' Generate 'Records' component of a 'Position List'
#'
#' @importFrom rlang .data
#'
#' @param data data.frame
#' @param ... ellipsis
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' map <- gen_pl_records(raw_data_frame)
#' }
gen_pl_records <- function(data, ...) {

  # Validate Inputs
  if (missing(data)) {stop("`data` is missing in call to `gen_pl_records`")}

  # Validate Input Expectations

  # * `data`
  if (!isTRUE(is.data.frame(data))) {
    stop("`data` must be type 'data.frame' in call to `gen_pl_records`")
  }

  if (!isTRUE(nrow(data) > 0)) {
    stop("`data` must have non-zero row count in call to `gen_pl_records`")
  }

  # Generate 'Map' and 'Index'
  sink("NUL")
  tmp <- invisible(gen_pl_index(data))
  sink()

  # Save 'index_names' for later use
  index_names <- names(tmp$index)

  # Generate 'Records'
  data_records <- lapply(index_names, function(x) {

    res <- tibble::enframe(tmp$index[[x]]) %>%
      tidyr::unnest(.data$value) %>%
      dplyr::arrange(.data$value) %>%
      dplyr::select(-.data$value)

    res[[x]] <- as.integer(res$name)
    res$name <- NULL
    as.data.frame(res)

  })
  names(data_records) <- index_names
  data_records <- dplyr::bind_cols(data_records)

  # Save Results
  result <- list(
    map = tmp$map,
    index = tmp$index,
    records = data_records
  )

  # Return Results
  return(result)

}
