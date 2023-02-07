
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
      data_x$index <- NULL

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
  return(as.data.frame(result))

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

#' Validate if an input object is 'pl_map'
#'
#' @param obj S3 Object
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- valid_pl_map(s3_obj)
#' }
valid_pl_map <- function(obj) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `valid_pl_map`")}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * is list?
  cat("is list?...")
  tictoc::tic()
  if (!isTRUE(is.list(obj))) {
    msg <- "`obj` must be a list"
    err <- c(msg, err)
  }
  tictoc::toc()

  # * does list have names?
  cat("does list have names?...")
  tictoc::tic()
  obj_names <- names(obj)
  obj_names_char <- isTRUE(is.character(obj_names))
  obj_names_na <- purrr::map_lgl(obj_names, function(x){isTRUE(is.na(x))})
  obj_names_null <- purrr::map_lgl(obj_names, function(x){isTRUE(is.null(x))})
  obj_names_empty <- isTRUE(any(obj_names_na)) || isTRUE(any(obj_names_null))
  if (!isTRUE(obj_names_char) && !isTRUE(obj_names_empty)) {
    msg <- "`names(obj)` must be character vector with no NA/NULL elements"
    err <- c(msg, err)
  }
  tictoc::toc()

  # Conditionally return 'err', if any errors at this point
  if (isTRUE(length(err) > 0)) {return(err)}

  # * are list elements data.frames?
  cat("are list elements data.frames?...")
  tictoc::tic()
  obj_elem_is_df <- purrr::map_lgl(obj, function(x){isTRUE(is.data.frame(x))})
  if (!isTRUE(all(obj_elem_is_df))) {
    msg <- "`obj` must only contain data.frames as elements"
    err <- c(msg, err)
  }
  tictoc::toc()

  # * do all list elements have correct colnames?
  cat("do all list elements have correct colnames?...")
  tictoc::tic()
  elem_colnames <- c('val', 'index')
  elem_valid_colnames <- purrr::map_lgl(obj, function(x){
    isTRUE(identical(sort(colnames(x)), sort(elem_colnames)))
  })
  if (!isTRUE(all(elem_valid_colnames))) {
    msg <- "`colnames(elem)` must equal `c('val', 'index')`"
    err <- c(msg, err)
  }
  tictoc::toc()

  # Conditionally return 'err', if any errors at this point
  if (isTRUE(length(err) > 0)) {return(err)}

  # * does each `obj$elem$index` make sense with `obj$elem$val`?
  cat("does each 'obj$elem$index' make sense with 'obj$elem$val'?...")
  tictoc::tic()
  index_val_match <- purrr::map_lgl(obj, function(x) {
    unq_val_cnt <- length(unique(x$val))
    res <- isTRUE(identical(as.integer(sort(1:unq_val_cnt)), as.integer(sort(x$index))))
    return(res)
  })
  if (!isTRUE(all(index_val_match))) {
    msg <- "`obj` elements must have valid column values"
    err <- c(msg, err)
  }
  tictoc::toc()

  # Return Final Output
  if (isTRUE(length(err) == 0)) {return(obj)}
  else {return(err)}

}

#' Validate if an input object is 'pl_index'
#'
#' @param obj S3 Object
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- valid_pl_index(obj)
#' }
valid_pl_index <- function(obj) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `valid_pl_index`")}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * is list?
  cat("is list?...")
  tictoc::tic()
  if (!isTRUE(is.list(obj))) {
    msg <- "`obj` must be a list"
    err <- c(msg, err)
  }
  tictoc::toc()

  # * does list have names?
  cat("does list have names?...")
  tictoc::tic()
  obj_names <- names(obj)
  obj_names_char <- isTRUE(is.character(obj_names))
  obj_names_na <- purrr::map_lgl(obj_names, function(x){isTRUE(is.na(x))})
  obj_names_null <- purrr::map_lgl(obj_names, function(x){isTRUE(is.null(x))})
  obj_names_empty <- isTRUE(any(obj_names_na)) || isTRUE(any(obj_names_null))
  if (!isTRUE(obj_names_char) && !isTRUE(obj_names_empty)) {
    msg <- "`names(obj)` must be character vector with no NA/NULL elements"
    err <- c(msg, err)
  }
  tictoc::toc()

  # Conditionally return 'err', if any errors at this point
  if (isTRUE(length(err) > 0)) {return(err)}

  # * do max-element values make sense with value sets?
  cat("do max-element values make sense with value sets?...")
  tictoc::tic()
  is_valid_valset <- purrr::map_lgl(obj, function(x) {

    valset_x <- purrr::reduce(x, `c`)
    unq_valset_x_cnt <- length(valset_x)
    max_valset_x <- max(valset_x)

    res <- isTRUE(identical(as.integer(sort(valset_x)), as.integer(1:unq_valset_x_cnt)))
    return(res)

  })
  if (!isTRUE(all(is_valid_valset))) {
    msg <- "`obj` elements must have valid column values"
    err <- c(msg, err)
  }
  tictoc::toc()

  # Return Final Output
  if (isTRUE(length(err) == 0)) {return(obj)}
  else {return(err)}

}

#' Validate if an input object is 'pl_records'
#'
#' @param obj S3 Object
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- valid_pl_records(obj)
#' }
valid_pl_records <- function(obj) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `valid_pl_records`")}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * is data.frame?
  cat("is data.frame?...")
  tictoc::tic()
  if (!isTRUE(is.list(obj))) {
    msg <- "`obj` must be a data.frame"
    err <- c(msg, err)
  }
  tictoc::toc()

  # * does data.frame have colnames?
  cat("does data.frame have colnames?...")
  tictoc::tic()
  obj_names <- colnames(obj)
  obj_names_char <- isTRUE(is.character(obj_names))
  obj_names_na <- purrr::map_lgl(obj_names, function(x){isTRUE(is.na(x))})
  obj_names_null <- purrr::map_lgl(obj_names, function(x){isTRUE(is.null(x))})
  obj_names_empty <- isTRUE(any(obj_names_na)) || isTRUE(any(obj_names_null))
  if (!isTRUE(obj_names_char) && !isTRUE(obj_names_empty)) {
    msg <- "`names(obj)` must be character vector with no NA/NULL elements"
    err <- c(msg, err)
  }
  tictoc::toc()

  # Conditionally return 'err', if any errors at this point
  if (isTRUE(length(err) > 0)) {return(err)}

  # * are all columns 'integer'?
  cat("are all columns 'integer'?...")
  tictoc::tic()
  is_col_integer <- purrr::map_lgl(obj, function(x) {isTRUE(is.integer(x))})
  if (!isTRUE(all(is_col_integer))) {
    msg <- "`obj` must only contain integer columns"
    err <- c(msg, err)
  }
  tictoc::toc()

  # * are all column values non-empty?
  cat("are all column values non-empty?...")
  tictoc::tic()
  is_non_empty <- purrr::map_lgl(obj, function(x){
    isTRUE(any(purrr::map_lgl(x, function(t){
      !isTRUE(is.na(t)) && !isTRUE(is.null(t))
    })))
  })
  if (!isTRUE(all(is_non_empty))) {
    msg <- "`obj` columns cannot contain empty cells"
    err <- c(msg, err)
  }
  tictoc::toc()

  # * are distinct values 'dense'?
  cat("are distinct values 'dense'?...")
  tictoc::tic()
  is_val_dense <- purrr::map_lgl(obj, function(x){
    unq_val_x <- unique(x)
    res <- isTRUE(identical(as.integer(sort(unq_val_x)), as.integer(1:length(unq_val_x))))
    return(res)
  })
  if (!isTRUE(all(is_val_dense))) {
    msg <- "`obj` column values must all be dense"
    err <- c(msg, err)
  }
  tictoc::toc()

  # Return Final Output
  if (isTRUE(length(err) == 0)) {return(obj)}
  else {return(err)}

}
