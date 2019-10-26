#' Summarizing xpdb
#'
#' @description This function returns a summary of an \code{\link{xpdb}} to the
#'   console.
#' @param object An \code{xpdb} object.
#' @param .problem The problem to be used, by default returns the last one for
#'   each label.
#' @param ... Ignored in this function
#'
#' @method summary xpdb_nm
#' @examples
#' summary(xpdb_ex_pk)
#'
#' @export
summary.xpdb_nm <- function(object, .problem = NULL, ...) {
  order <- c('software', 'version', 'dir', 'file', 'run', 'ref', 'descr', 'timestart', 
             'timestop', 'probn', 'label', 'data', 'nind', 'nobs', 'subroutine', 'method',
             'term', 'runtime', 'ofv', 'nsig', 'covtime', 'condn','etashk', 'epsshk', 
             'nsim', 'simseed', 'nesample', 'esampleseed', 'errors', 'warnings')
  
  out <- get_summary(object, .problem, only_last = FALSE) %>% 
    dplyr::filter(.$value != 'na') %>% 
    dplyr::slice(order(match(.$label, order))) %>% 
    dplyr::group_by_at(.vars = c('problem', 'label', 'descr')) %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(value = purrr::map_chr(.$data, function(x) {
      if (nrow(x) == 1) return(x$value)
      value <- stringr::str_c(x$value, ' (subprob no.', x$subprob, ')', sep = '')
      stringr::str_c(value, collapse = '\n')
    })) %>% 
    dplyr::mutate(descr = stringr::str_pad(.$descr, width = max(nchar(.$descr)) + 2, 'right'),
                  label = stringr::str_pad(.$label, width = max(nchar(.$label)), 'right'),
                  value = stringr::str_replace_all(.$value, '\n', stringr::str_pad('\n', max(nchar(.$descr)) + max(nchar(.$label)) + 9, 'right'))) %>% 
    dplyr::mutate(descr = stringr::str_c(.$descr, '@', .$label, '')) %>% 
    dplyr::mutate(string = stringr::str_c(' -', .$descr, ':', .$value, sep = ' '),
                  grouping = as.character(.$problem)) %>% 
    dplyr::group_by_at(.vars = 'grouping') %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    {purrr::map(.$data, function(x) {
      x <- dplyr::filter(.data = x, !stringr::str_detect(x$descr, 'Problem number'))
      if (x$problem[1] == 0) {
        lab <- '[Global information]'
      } else {
        lab_row <- which(stringr::str_detect(x$descr, stringr::fixed('Run label')))
        lab <- stringr::str_c('[', x$value[lab_row], ']', sep = '')
        x <- x[-lab_row, ]
      }
      
      cat('\nSummary for problem no.', x$problem[1], lab, '\n')
      cat(x$string, sep = '\n')
    })}
}
