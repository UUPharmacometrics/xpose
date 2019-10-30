#' List available datasets
#'
#' @description Function providing a detailed listing of all available datasets in an xpdb object.
#' 
#' @param xpdb An \code{xpdb} object to be evaluated
#' @seealso \code{\link{get_data}}, \code{\link{get_file}}, \code{\link{get_special}}
#' @examples
#' # List output tables data
#' list_data(xpdb_ex_pk)
#' 
#' # List output files data
#' list_files(xpdb_ex_pk)
#' 
#' # List special data
#' xpdb_ex_pk %>% 
#' vpc_data(quiet = TRUE) %>% 
#' list_special()
#' 
#' @name list_xpdb
#' @export
list_data <- function(xpdb) {
  check_xpdb(xpdb, check = 'data')
  xpdb$data %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'data', 'modified')) %>% 
    {utils::capture.output(print(.))} %>% 
    {c('Data:', .[-1])} %>% 
    cat(sep = '\n')
}

#' @name list_xpdb
#' @export
list_files <- function(xpdb) {
  check_xpdb(xpdb, check = 'files')
  xpdb$files %>% 
    dplyr::select(dplyr::one_of('name', 'extension', 'problem', 'subprob', 'method', 'data', 'modified')) %>% 
    {utils::capture.output(print(.))} %>% 
    {c('Files:', .[-1])} %>% 
    cat(sep = '\n')
}

#' @name list_xpdb
#' @export
list_special <- function(xpdb) {
  check_xpdb(xpdb, check = 'special')
  xpdb$special %>% 
    dplyr::select(dplyr::one_of('problem', 'method', 'type', 'data', 'modified')) %>% 
    {utils::capture.output(print(.))} %>% 
    {c('Specials:', .[-1])} %>% 
    cat(sep = '\n')
}
