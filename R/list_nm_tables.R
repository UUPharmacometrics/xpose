#' List NONMEM output tables
#'
#' @description List NONMEM output tables file names from a \code{nm_model}
#'   object.
#'
#' @param nm_model An xpose nm_model object generated with
#'   \code{\link{read_nm_model}}.
#'
#' @seealso \code{\link{read_nm_model}}, \code{\link{read_nm_tables}}
#' @examples
#' \dontrun{
#' read_nm_model(file = 'run001.lst') %>%
#'   list_nm_tables()
#' }
#'
#' @export
list_nm_tables <- function(nm_model = NULL) {
  
  if (is.null(nm_model) || !is.nm.model(nm_model)) {
    stop('Object of class `nm_model` required.', call. = FALSE)
  }
  
  # Prepare null object to be returned if no $TABLE is found
  null_object <- as.table.list.nm(tibble::tibble(problem = -1, file = '', 
                                                 firstonly = NA, simtab = NA))
  
  # Get the parsed NM code associated with the tables
  table_list <- nm_model %>% 
    purrr::pluck('data') %>% 
    dplyr::filter(!!rlang::sym('parsed')) %>% 
    purrr::pluck('code', 1) %>% 
    dplyr::filter(!!rlang::sym('problem') > 0, 
                  !!rlang::sym('subroutine') == 'tab') 
  
  if (nrow(table_list) == 0) return(null_object)
  
  # Get the model directory
  model_dir <- nm_model %>% 
    purrr::pluck('file_info') %>% 
    dplyr::pull(!!rlang::sym('file')) %>% 
    dirname()
  
  # Search the model code for table files
  table_list <- table_list %>% 
    dplyr::group_by_at(.vars = c('problem', 'level')) %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(string = purrr::map_chr(.x = !!rlang::sym('data'), 
                                          .f = ~stringr::str_c(.x$code, collapse = ' '))) %>% 
    dplyr::mutate(file = stringr::str_match(string = !!rlang::sym('string'), 
                                            pattern = '(^|\\s+)FILE\\s*=\\s*([^\\s]+)')[, 3]) %>% 
    tidyr::drop_na(dplyr::one_of('file'))
  
  if (nrow(table_list) == 0) return(null_object)
  
  # Finalize file name prep and find firstonly option
  table_list <- table_list %>% 
    dplyr::mutate(file      = file_path(model_dir, !!rlang::sym('file')),
                  firstonly = stringr::str_detect(!!rlang::sym('string'), 'FIRSTONLY')) %>% 
    dplyr::select(dplyr::one_of('problem', 'file', 'firstonly'))
  
  # Prep simtab flag
  sim_flag <- nm_model %>% 
    purrr::pluck('data') %>% 
    dplyr::filter(!!rlang::sym('parsed')) %>% 
    purrr::pluck('code', 1) %>%
    dplyr::filter(!!rlang::sym('problem') > 0) %>% 
    dplyr::group_by_at(.vars = 'problem') %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(simtab = purrr::map_lgl(.x = !!rlang::sym('data'), 
                                          .f = ~!any(stringr::str_detect(.x$subroutine, 'est')))) %>% 
    dplyr::select(dplyr::one_of(c('problem', 'simtab')))
  
  # Merge and output
  table_list %>% 
    dplyr::left_join(sim_flag, by = 'problem') %>% 
    as.table.list.nm()
}
