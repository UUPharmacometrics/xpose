#' Get variable type, label or units
#'
#' @description Function designed to extract the type, label or unit associated
#' with variables
#' 
#' @param xpdb An \code{xpose_data} object.
#' @param .problem The problem number to which the edits will be applied.
#' @param variable A vector of data variables
#'  
#' @return A character vector
#' @examples
#' # Get variable types
#' get_var_types(xpdb_ex_pk, .problem = 1, c('ID', 'MED1')
#' 
#' # Get variable labels
#' get_var_labels(xpdb_ex_pk, .problem = 1, c('ID', 'MED1')
#' 
#' # Get variable units
#' get_var_units(xpdb_ex_pk, .problem = 1, c('ID', 'MED1')
#' 
#' @name get_vars
#' @export
#' 
get_var_types <- function(xpdb, ..., .problem = NULL) {
  get_var_generic(
    xpdb = xpdb, .problem = .problem, what = 'type', ...
  )
}

#' @rdname get_vars
#' @export
get_var_labels <- function(xpdb, ..., .problem = NULL) {
  tmp <- get_var_generic(
    xpdb = xpdb, .problem = .problem, what = 'label', ...
  )
  if ( any(is.na(tmp)) ){
    tmp[is.na(tmp)] <- names(tmp)[is.na(tmp)] 
  }
  tmp
}

#' @rdname get_vars
#' @export
get_var_units <- function(xpdb, ..., .problem = NULL) {
  get_var_generic(
    xpdb = xpdb, .problem = .problem, what = 'units', ...
  )
}


get_var_generic <- function(xpdb, .problem = NULL, what = NULL, ...) {
  
  check_xpdb(xpdb, check = 'data')
  if (is.null(.problem)) 
    .problem <- default_plot_problem(xpdb)
  if (!is.null(.problem) && !all(.problem %in% xpdb$data$problem)) {
    stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb$data$problem], collapse = ', '), 
         ' not found in model output data.', call. = FALSE)
  }
  
  variables <- c(...)
  
  if (is.null(variables))
    return(NULL)
  if (is.null(what))
    return(NULL)
  
  tmp <- xpdb$data$index[[.problem]] %>% 
    dplyr::filter( col %in% variables ) %>% 
    dplyr::left_join(
      data.frame(
        order_ = 1:length(variables),
        col = variables
      ),
      by = 'col'
    ) %>% 
    dplyr::arrange(order_)
  
  if ( what == 'type' ){
    res <- tmp %>% dplyr::pull( type )
  } else if ( what == 'label' ){
    res <- tmp %>% dplyr::pull( label )
  } else if ( what == 'units' ){
    res <- tmp %>% dplyr::pull( units )
  } else {
    return(NULL)
  }
  names(res) <- tmp %>% dplyr::pull( col )
  if (length(res) == 0)
    res <- NULL
  res
}

#' Extract variable labels and units
#' 
#' @description Utility function to construct a string made of a variable label 
#' and units, if present in xpose object. Units are surrounded by parenthesis or
#' sqaure brackets based upon 'xpose_square_bracket' option.
#' 
#' @param xpdb An xpose database object.
#' @param .problem The $problem number to be used.
#' @param variable Variable(s) for which labels and units are to be extracted.
#' 
#' @return A character string.
#' 
#' @examples
#' labels_units <- data.frame(
#'   col = c('ALAG1', 'CL', 'V'),
#'   label = c('Lag time', 'Clearance', 'Volume'),
#'   units = c('h', 'L/h', 'L')
#' )
#' 
#' get_var_labels_units(xpdb, 'ALAG1')
#' get_var_labels_units(xpdb, 'ALAG1', 'CL')
#' 
#' @keywords internal
#' @export

get_var_labels_units <- function(xpdb, ..., .problem = NULL){
  
  check_xpdb(xpdb, check = 'data')
  
  variables <- c(...)
  
  if ( !is.null(.problem) ){
    tmp <- data.frame(
      label = get_var_labels( xpdb = xpdb, .problem = .problem, ... ),
      units = get_var_units( xpdb = xpdb, .problem = .problem, ... )
    )
    tmp <- tmp %>% 
      dplyr::mutate( col = row.names(tmp) )
  } else {
    if ( is.null(xpdb$label_units) ){
      return(variables)
    }
    tmp <- xpdb$label_units %>% 
      dplyr::filter( col %in% variables ) %>% 
      dplyr::left_join(
        data.frame(
          order_ = 1:length(variables),
          col = variables
        ),
        by = 'col'
      ) %>% 
      dplyr::mutate( label = ifelse(is.na(label), col, label) ) %>% 
      dplyr::arrange(order_)
  }
  
  if ( nrow(tmp)==0 )
    return(NULL)
  
  open_sep <- ifelse( isTRUE(xpdb$options$square_bracket), '[', '(')
  close_sep <- ifelse( isTRUE(xpdb$options$square_bracket), ']', ')')
  
  tmp <- tmp %>% 
    dplyr::mutate(
      label_units = ifelse(
        is.na(units),
        label,
        paste0(label,' ', open_sep, units, close_sep)
      )
    )
  
  res <- tmp %>% dplyr::pull( label_units )
  names(res) <- tmp %>% dplyr::pull( col )
  res
  
}
