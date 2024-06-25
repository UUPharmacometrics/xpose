#' Subset datasets in an xpdb
#' 
#' @description Use \code{filter()} to select rows/cases where conditions are true. 
#' Unlike base subsetting, rows where the condition evaluates to NA are dropped.
#' Use \code{slice()} to select row/cases by their position
#' 
#' @inheritParams edit_xpose_data
#' @method filter xpose_data
#' @examples
#' # Subset by condition
#' xpdb_ex_pk %>% 
#'  filter(DV < 1, .problem = 1) %>% 
#'  dv_vs_ipred()
#'   
#' # Subset by positions
#' xpdb_ex_pk %>% 
#'  slice(1:100, .problem = 1) %>% 
#'  dv_vs_ipred()
#'  
#' # Deduplicate rows
#' xpdb_ex_pk %>% 
#'  distinct(TIME, .problem = 1) %>% 
#'  dv_vs_ipred()
#' @name subset_xpdb
#' @export
filter.xpose_data <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::filter, .fname = 'filter', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}


#' @method slice xpose_data
#' @name subset_xpdb
#' @export
slice.xpose_data <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::slice, .fname = 'slice', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}


#' @method distinct xpose_data
#' @name subset_xpdb
#' @export
distinct.xpose_data <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = function(.data, ...) {dplyr::distinct(.data, ..., .keep_all = TRUE)}, 
                  .fname = 'distinct', .data = .data, .problem = .problem, 
                  .source = .source, .where = .where, ...)
}


#' Add, remove or rename variables in an xpdb
#' 
#' @description \code{mutate()} adds new variables and preserves existing ones. 
#' \code{select()} keeps only the listed variables; \code{rename()} keeps all variables.
#' 
#' @inheritParams edit_xpose_data
#' @method mutate xpose_data
#' @examples
#' # Mutate columns
#' xpdb_ex_pk %>% 
#'  mutate(lnDV = log(DV), 
#'         sim_count = irep(ID), 
#'         .problem = 1) %>% 
#'  dv_vs_idv(aes(y = lnDV))
#'  
#' # Rename/select columns
#' xpdb_ex_pk %>% 
#'  select(ID:TAD, DV, EVID) %>% 
#'  rename(TSLD = TAD) %>% 
#'  dv_vs_idv(aes(x = TSLD))
#' @name modify_xpdb
#' @export
mutate.xpose_data <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}


#' @method select xpose_data
#' @name modify_xpdb
#' @export
select.xpose_data <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::select, .fname = 'select', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}


#' @method rename xpose_data
#' @name modify_xpdb
#' @export
rename.xpose_data <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::rename, .fname = 'rename', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}


#' Group/ungroup and summarize variables in an xpdb
#' 
#' @description \code{group_by()} takes an existing table and converts it into a 
#' grouped table where operations are performed "by group". \code{ungroup()} removes grouping.
#' \code{summarize()} reduces multiple values down to a single value.
#' 
#' @inheritParams edit_xpose_data 
#' @param x Same as .data (used for consistency with dplyr functions).
#' @method group_by xpose_data
#' @examples
#' # Create a distribution plot of Cmax
#' xpdb_ex_pk %>% 
#'  group_by(ID, SEX, .problem = 1) %>% 
#'  summarize(CMAX = max(DV), .problem = 1) %>% 
#'  ungroup(.problem = 1) %>% 
#'  xplot_distrib(aes(x = CMAX, density_fill = SEX), type = 'dr')
#'  
#' @name summarise_xpdb
#' @export
group_by.xpose_data <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::group_by, .fname = 'group_by', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}


#' @method ungroup xpose_data
#' @name summarise_xpdb
#' @export
ungroup.xpose_data <- function(x, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::ungroup, .fname = 'ungroup', .data = x,
                  .problem = .problem, .source = .source, .where = .where, ...)
}


#' @method summarise xpose_data
#' @name summarise_xpdb
#' @export
summarise.xpose_data <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::summarise, .fname = 'summarise', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}

#' @method summarize xpose_data
#' @name summarise_xpdb
#' @export
summarize.xpose_data <- summarise.xpose_data


#' Master xpdb editing function
#' 
#' @description Generic function used to build dedicated editing functions
#' 
#' @param .fun An editing function to be applied to the data.
#' @param .fname The name of the editing function.
#' @param .data An xpose database object.
#' @param .problem The problem from which the data will be modified
#' @param .source The source of the data in the xpdb. Can either be 'data' or an output 
#' file extension e.g. 'phi'.
#' @param .where A vector of element names to be edited in special (e.g. 
#' \code{.where = c('vpc_dat', 'aggr_obs')} with vpc).
#' @param ... Name-value pairs of expressions. Use \code{NULL} to drop a variable.
#' 
#' These arguments are automatically quoted and evaluated in the 
#' context of the data frame. They support unquoting and splicing. 
#' See the dplyr vignette("programming") for an introduction to these concepts.
#' @keywords internal
#' @export
edit_xpose_data <- function(.fun, .fname, .data, ..., .problem, .source, .where) {
  
  # Check input
  xpdb <- .data # Avoids issues with dplyr arguments
  if (missing(.source)) .source <- 'data'
  if (length(.source) > 1) stop('Argument `.source` should be of length 1.', call. = FALSE)
  check_xpdb(xpdb, check = .source)
  
  # Direct filter to specified source
  if (.source == 'data') {
    if (missing(.problem)) .problem <- all_data_problem(xpdb)
    if (!all(.problem %in% all_data_problem(xpdb))) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['data']]$problem], collapse = ', '), 
           ' not found in model output data.', call. = FALSE)
    }
    
    check_quo_vars(xpdb = xpdb, ..., .source = .source, .problem = .problem)
    
    # do dplyr operation outside of mutate to avoid problems with n()
    xpdb[['data']]$data <- purrr::map_if(xpdb[['data']]$data, xpdb[['data']]$problem %in% .problem,
                                         .f = .fun, !!!rlang::enquos(...))
    xpdb[['data']] <- xpdb[['data']] %>%
      dplyr::mutate(modified = dplyr::if_else(.$problem %in% .problem, TRUE, .$modified))
    
    if (.fname %in% c('mutate', 'select', 'rename')) {
      xpdb[['data']] <- xpdb_index_update(xpdb = xpdb, .problem = .problem) # Update index
    }
  } else if (.source == 'special') {
    if (missing(.problem)) {
      .problem <- max(xpdb[['special']]$problem)
      msg(c('Changes will be applied to `special` $prob no.', .problem), quiet = FALSE)
    }
    if (!all(.problem %in% xpdb[['special']]$problem)) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['special']]$problem], collapse = ', '), 
           ' not found in `special` data.', call. = FALSE)
    }
    
    check_quo_vars(xpdb = xpdb, ..., .source = .source, .problem = .problem)
    
    xpdb[['special']] <- xpdb[['special']] %>%
      dplyr::group_by_at(.vars = 'problem')
    
    ## TEMP handling
    if (tidyr_new_interface()) {
      xpdb[['special']] <- xpdb[['special']] %>% 
        tidyr::nest(tmp = -dplyr::one_of('problem')) %>% 
        dplyr::ungroup()
    } else {
      xpdb[['special']] <- xpdb[['special']] %>% 
        tidyr::nest(.key = 'tmp') %>% 
        dplyr::ungroup()
    }
    ## END TEMP
    
    xpdb[['special']]$tmp <- purrr::map_if(.x = xpdb[['special']]$tmp, .p = xpdb[['special']]$problem %in% .problem,
                                           .f = function(.x, .fun, .where, ...) {
                                             if (.x$method == 'vpc') {
                                               if (any(!.where %in% names(.x$data[[1]]))) {
                                                 warning('elements ', stringr::str_c(.where[!.where %in% names(.x$data[[1]])], collapse = ', '), 
                                                         ' not found in ', .x$method, ' ', .x$type, call. = FALSE)
                                               }
                                               .x$data[[1]] <- .x$data[[1]] %>% 
                                                 purrr::map_at(.at = .where, .f = .fun, ...)
                                               .x$modified <- TRUE
                                               return(.x)
                                             } else {
                                               stop('edits of `', .x$method, '` data are not yet supported in xpose.', call. = FALSE)
                                             }
                                           }, .fun = .fun, .where = .where, !!!rlang::enquos(...)) 
    
    xpdb[['special']] <- xpdb[['special']] %>% 
      tidyr::unnest(dplyr::one_of('tmp'))
  } else {
    if (missing(.problem)) .problem <- max(xpdb[['files']]$problem)
    if (!all(.source %in% xpdb[['files']]$extension)) {
      stop('File extension ', stringr::str_c(.source[!.source %in% xpdb[['files']]$extension], collapse = ', '), 
           ' not found in model output files.', call. = FALSE)
    }
    
    if (!all(.problem %in% xpdb[['files']]$problem[xpdb[['files']]$extension %in% .source])) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['files']]$problem], collapse = ', '), 
           ' not found in model output files.', call. = FALSE)
    }
    
    check_quo_vars(xpdb = xpdb, ..., .source = .source, .problem = .problem)
    
    xpdb[['files']]$data <- purrr::map_if(.x = xpdb[['files']]$data, .p = xpdb[['files']]$problem %in% .problem &
                                            xpdb[['files']]$extension %in% .source,
                                          .f = .fun, !!!rlang::enquos(...))
    xpdb[['files']] <- xpdb[['files']] %>%
      dplyr::mutate(modified = dplyr::if_else(.$problem %in% .problem & .$extension %in% .source, TRUE, .$modified))
  }
  as.xpdb(xpdb) 
}


#' Update data index
#' 
#' @description Function dedicated to update the data index after modifications.
#' 
#' @param xpdb An xpose database object from which the index will be updated.
#' @param .problem The problem from which the index will be updated.
#' 
#' @keywords internal
#' @export
xpdb_index_update <- function(xpdb, .problem) {
  dat <- xpdb[['data']] %>% 
    dplyr::group_by_at(.vars = 'problem')
  
  ## TEMP handling
  if (tidyr_new_interface()) {
    dat <- dat %>% tidyr::nest(tmp = -dplyr::one_of('problem'))
  } else {
    dat <- dat %>% tidyr::nest(.key = 'tmp')
  }
  ## END TEMP
  
  dat %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(tmp = purrr::map_if(.$tmp, 
                                      xpdb[['data']]$problem %in% .problem,
                                      function(x) {
                                        col_names <- colnames(x$data[[1]])
                                        # Drop columns not present in data anymore
                                        x$index[[1]] <- x$index[[1]] %>% 
                                          dplyr::filter(.$col %in% col_names)
                                        
                                        # Add new columns found in data
                                        add_cols <- col_names[!col_names %in% x$index[[1]]$col]
                                        if (length(add_cols) > 0) {
                                          x$index[[1]] <- x$index[[1]] %>%   
                                            dplyr::bind_rows(
                                              dplyr::tibble(table = 'na', col = add_cols, type = 'na', 
                                                            label = NA_character_, 
                                                            units = NA_character_))
                                        }
                                        x
                                      })) %>% 
    tidyr::unnest(dplyr::one_of('tmp'))
}


#' Check quoted variables
#' 
#' @description Ensure that variables used in quos are present in the 
#' data and return informative error messages otherwise.
#' 
#' @param xpdb An xpose database object.
#' @param ... Name-value pairs of expressions.
#' @param .problem The problem from which the data will be modified
#' @param .source The source of the data in the xpdb. Can either be 'data' or an output 
#' file extension e.g. 'phi'.
#' 
#' @return Silent if checks are successful, returns errors otherwise.
#' 
#' @keywords internal
#' @export
check_quo_vars <- function(xpdb, ..., .source, .problem) {
  if (.source == 'special') return(invisible())
  
  quo_vars <- rlang::enquos(...) %>% 
    purrr::map(all.vars) %>% 
    purrr::flatten_chr()
  
  if (length(quo_vars) > 0) {
    if (.source == 'data') {
      tmp <- xpdb$data[xpdb$data$problem %in% .problem, ] 
    } else {
      tmp <- xpdb$files[xpdb$files$extension %in% .source & xpdb$files$problem %in% .problem, ]
    }
    
    tmp <- dplyr::mutate(.data = tmp,
                         missing = purrr::map(tmp$data, ~stringr::str_c(quo_vars[!quo_vars %in% colnames(.)], collapse = ', ')),
                         error = purrr::map_lgl(tmp$data, ~any(!quo_vars %in% colnames(.))))
    
    if (any(tmp$error)) {
      tmp %>% 
        dplyr::filter(.$error) %>% 
        dplyr::mutate(string = stringr::str_c('missing: ', .$missing, ' variables in ', 
                                              ifelse(.source == 'data', '', stringr::str_c('`', .source, '` file ')), 
                                              '$prob no.', .$problem, '.')) %>% 
        {stop(stringr::str_c(.$string, collapse = '\n       '), call. = FALSE)}
    }
  }
}


#' Add simulation counter
#'
#' @description Add a column containing a simulation counter (irep). A new simulation is counted everytime
#' a value in x is lower than its previous value.
#' 
#' @param x The column to be used for computing simulation number, usually the ID column.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @examples
#' xpdb_ex_pk_2 <- xpdb_ex_pk %>% 
#'  mutate(sim_id = irep(ID), .problem = 2)
#' 
#' @export
irep <- function(x, quiet = FALSE) {
  if (missing(x)) stop('argument "x" is missing, with no default', call. = FALSE)
  if (is.factor(x)) x <- as.numeric(as.character(x))
  lagcheck <- dplyr::lag(x, default = x[1]) != x
  dupcheck <- duplicated(x)
  check <- dplyr::if_else(lagcheck & dupcheck, 1, 0)
  ilen <- which(check==1)[1] - 1
  if (length(ilen)==0 || is.na(ilen)) ilen <- length(x)
  x <- rep(1:(length(x)/ilen), each=ilen)
  msg(c('irep: ', max(x), ' simulations found.'), quiet)
  x
}
