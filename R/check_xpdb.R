#' Check xpdb source files
#'
#' @description These functions check the status of the data in an xpdb.
#'   \code{\link{check_xpdb_source}} checks that the files used to build
#'   the xpdb have not been changed. \code{\link{check_xpdb_changes}} checks
#'   whether the xpdb content has been edited since its creation.
#'
#' @param xpdb An \code{xpdb} object generated with
#'   \code{\link{create_nm_xpdb}}.
#'
#' @examples
#' # Check for changes in the source files
#' check_xpdb_source(xpdb_ex_pk)
#'
#' # Check for changes in the xpdb
#' check_xpdb_changes(xpdb_ex_pk)
#'
#' @name check_xpdb
#' @export
check_xpdb_source <- function(xpdb) {
  
  # Check input
  check_xpdb(xpdb, check = 'file_info')
  
  # Check the files md5
  xpdb %>% 
    purrr::pluck('file_info') %>% 
    dplyr::mutate(status = purrr::map2_chr(
      .x = !!rlang::sym('file'), 
      .y = !!rlang::sym('md5'),
      .f = ~.x %>% 
        tools::md5sum() %>% 
        {dplyr::case_when(is.na(.) ~ 'Unknown',
                         . != .y  ~ 'Outdated',
                         TRUE     ~ 'Up to date')})) %>% 
    structure(class = c('xpdb_check_source', class(.)))
}

#' @rdname check_xpdb
#' @export
check_xpdb_changes <- function(xpdb) {
  # Check input
  check_xpdb(xpdb, check = 'file_info')
} 

