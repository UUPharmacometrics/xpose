#' Combine xpdb objects
#'
#' @description This function combines several \code{\link{xpdb}} object into a
#'   single one \code{\link{xpdb}}.
#'
#' @param ... \code{\link{xpdb}} objects to be combined
#' @details Only \code{\link{xpdb}} based on a single modeling software (e.g.
#' NONMEM, nlmixr) can be combined together. The themes (ggtheme, xptheme) and
#' options (i.e. quiet) will be taken from the first xpdb element.
#' @method c xpdb
#' @examples
#' xpdb_combined <- c(xpdb_ex_pk, xpdb_ex_pk)
#' xpdb_combined
#' 
#' @export
c.xpdb <- function(...) {
  # Capture the dots
  x <- list(...)
  
  # Check inputs
  if (!all(purrr::map_lgl(.x = x, .f = is.xpdb))) {
    stop('Only `xpdb` objects can be combined.', call. = FALSE)
  }
  
  if (length(unique((purrr::map_chr(.x = x, .f = software)))) > 1) {
    stop('Only `xpdb` objects based on a single modeling software can be combined.', 
         call. = FALSE)
  }
  
  # Combine xpdbs
  out <- list(code       = c_xpdb_internal(x, item = 'code'), 
              summary    = c_xpdb_internal(x, item = 'summary'), 
              data       = c_xpdb_internal(x, item = 'data'),
              files      = c_xpdb_internal(x, item = 'files'),
              file_info  = c_xpdb_internal(x, item = 'file_info'),
              gg_theme   = x[[1]]$gg_theme, 
              xp_theme   = x[[1]]$xp_theme,
              options    = x[[1]]$options)
  
  # Get new mod num
  new_num <- out %>% 
    purrr::keep(.p = tibble::is_tibble) %>% 
    purrr::map(.f = ~dplyr::distinct_at(.tbl  = .x,
                                        .vars = dplyr::vars('rank', 'mod_num'))) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct_all() %>% 
    dplyr::mutate(mod_num2 = 1:n())
  
  # Update mod_nums
  out %>% 
    purrr::map_if(.p = tibble::is_tibble,
                  .f = ~.x %>% 
                    dplyr::mutate(sort_col = 1:n()) %>% 
                    dplyr::left_join(y  = .y, 
                                     by = c('rank', 'mod_num')) %>% 
                    dplyr::arrange(!!rlang::sym('sort_col')) %>% 
                    dplyr::mutate(mod_num = !!rlang::sym('mod_num2')) %>% 
                    dplyr::select(-dplyr::one_of('rank', 'mod_num2', 'sort_col')),
                  .y = new_num) %>% 
    
    # Ensure that the input class is preserved
    structure(class = class(x[[1]]))
}

# Helper function to add a counter (i.e. rank) to a given xpdb item (e.g. data)
c_xpdb_internal <- function(x, item) {
  purrr::map2(
    .x = x, 
    .y = seq_along(x), 
    .f = ~ .x %>% 
      purrr::pluck(item) %>% 
      mutate(rank = .y)) %>% 
    dplyr::bind_rows()
}
