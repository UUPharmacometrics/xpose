#' List available variables
#'
#' @description Function listing all available variables in an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model code will be extracted.
#' @param .problem The problem to be used, by lists all available problems.
#' 
#' @seealso \code{\link{set_var_types}}
#' @examples
#' list_vars(xpdb_ex_pk)
#' @export
list_vars <- function(xpdb, .problem = NULL) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  
  x <- xpdb$data
  
  if (!is.null(.problem)) {
    if (!all(.problem %in% x$problem)) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% x$problem], collapse = ', '), 
           ' not found in the data.', call. = FALSE)
    }
    x <- x[x$problem %in% .problem, ]
  }
  
  order <- c('id', 'dv', 'idv', 'tad', 'dvid', 'occ', 'amt', 'evid', 'mdv', 
             'pred', 'ipred', 'param', 'eta', 'res', 'catcov', 'contcov', 'a',
             'na')
  
  x <- x %>% 
    dplyr::mutate(grouping = as.integer(.$problem)) %>% 
    dplyr::group_by_at(.vars = 'grouping') %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    {purrr::map(.$data, function(df) {
      cat('\nList of available variables for problem no.', df$problem[1], '\n')
      df$index[[1]] %>% 
        dplyr::group_by_at(.vars = 'type') %>% 
        tidyr::nest() %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(string = purrr::map_chr(.$data, ~stringr::str_c(unique(.$col), collapse = ', ')),
                      descr = dplyr::case_when(.$type == 'id' ~ 'Subject identifier (id)',
                                               .$type == 'occ' ~ 'Occasion flag (occ)',
                                               .$type == 'na' ~ 'Not attributed (na)',
                                               .$type == 'amt' ~ 'Dose amount (amt)',
                                               .$type == 'idv' ~ 'Independent variable (idv)',
                                               .$type == 'tad' ~ 'Time after dose (tad)',
                                               .$type == 'ipred' ~ 'Model individual predictions (ipred)',
                                               .$type == 'pred' ~ 'Model typical predictions (pred)',
                                               .$type == 'res' ~ 'Residuals (res)',
                                               .$type == 'evid' ~ 'Event identifier (evid)',
                                               .$type == 'dv' ~ 'Dependent variable (dv)',
                                               .$type == 'catcov' ~ 'Categorical covariates (catcov)',
                                               .$type == 'contcov' ~ 'Continuous covariates (contcov)',
                                               .$type == 'param' ~ 'Model parameter (param)',
                                               .$type == 'eta' ~ 'Eta (eta)',
                                               .$type == 'a' ~ 'Compartment amounts (a)',
                                               .$type == 'dvid' ~ 'DV identifier (dvid)',
                                               .$type == 'mdv' ~ 'Missing dependent variable (mdv)')) %>% 
        dplyr::mutate(descr = stringr::str_pad(.$descr, 37, 'right')) %>% 
        dplyr::slice(order(match(.$type, order))) %>% 
        {stringr::str_c(' -', .$descr, ':', .$string, sep = ' ')} %>% 
        cat(sep = '\n')})}
}
