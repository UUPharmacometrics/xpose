#' Print an xpdb object
#'
#' @description This function returns to the console a list of the files and
#'   options attached to an \code{\link{xpdb}} object.
#'
#' @param x An \code{xpdb} object.
#' @param ... Ignored in this function
#'
#' @method print xpdb
#' @examples
#' # Using the print function
#' print(xpdb_ex_pk)
#'
#' # Or simply by writting the xpdb name
#' xpdb_ex_pk
#'
#' @export
print.xpdb <- function(x, ...) {
  
  # Summarize estimation tables names
  if (!is.null(x$data) && any(!x$data$simtab)) {
    tab_names <- x$data %>% 
      dplyr::filter(.$simtab == FALSE) %>% 
      dplyr::mutate(grouping = 1:n()) %>% 
      dplyr::group_by_at(.vars = 'grouping') %>% 
      tidyr::nest() %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(string = purrr::map_chr(.$data, summarize_table_names)) %>% 
      {stringr::str_c(.$string, collapse = '\n               ')}
  } else {
    tab_names <- '<none>'
  }
  
  # Summarize simulation tables names
  if (!is.null(x$data) && any(x$data$simtab)) {
    sim_names <- x$data %>% 
      dplyr::filter(.$simtab == TRUE) %>% 
      dplyr::mutate(grouping = 1:n()) %>% 
      dplyr::group_by_at(.vars = 'grouping') %>% 
      tidyr::nest() %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(string = purrr::map_chr(.$data, summarize_table_names)) %>% 
      {stringr::str_c(.$string, collapse = '\n               ')}
  } else {
    sim_names <- '<none>'
  }
  
  # Summarize file names
  if (!is.null(x$files)) {
    out_names <- x$files %>% 
      dplyr::distinct(!!rlang::sym('name'), .keep_all = TRUE) %>% 
      dplyr::arrange_at(.vars = 'name') %>% 
      {stringr::str_c(.$name, ifelse(.$modified, ' (modified)', ''), collapse = ', ')}
  } else {
    out_names <- '<none>'
  }
  
  # Summarize special table names
  if (!is.null(x$special)) {
    special_names <- stringr::str_c(x$special$method, ' ', x$special$type, 
                                    ' (#', x$special$problem,
                                    ifelse(x$special$modified, ', modified', ''),
                                    ')', collapse = ', ')
  } else {
    special_names <- '<none>'
  }
  
  # Summarize options
  opt_names <- x$options %>% 
    purrr::map_if(.p = is.null, 
                  .f = function(x) {'NULL'}) %>% 
                  {stringr::str_c(names(.), unlist(.), sep = ' = ', collapse = ', ')}
  
  cat(x$summary$value[x$summary$label == 'file'], 'overview:',
      '\n - Software:', x$summary$value[x$summary$label %in% c('software', 'version') & x$summary$value != 'na'],
      stringr::str_c('\n - Attached files (memory usage ', format(utils::object.size(x), units = 'auto'), '):'),  
      '\n   + obs tabs:', tab_names,
      '\n   + sim tabs:', sim_names,
      '\n   + output files:', out_names,
      '\n   + special:', special_names,
      '\n - gg_theme:', attr(x$gg_theme, 'theme'),
      '\n - xp_theme:', attr(x$xp_theme, 'theme'),
      '\n - Options:', opt_names)
}

summarize_table_names <- function(dat) {
  purrr::map(dat$index, ~.$table) %>% 
    purrr::flatten_chr() %>% 
    sort() %>% 
    unique() %>% 
    stringr::str_c(collapse = ', ') %>% 
    {stringr::str_c('$prob no.', dat$problem, 
                    '', #ifelse(dat$modified, ' (modified)', ''), # Add check_xpdb_changes
                    ': ', .)}
}
