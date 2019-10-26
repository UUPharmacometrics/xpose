#' NONMEM output table import function
#'
#' @description Quickly import NONMEM output tables into R. This function
#'   automatically detects the optimal settings to import the tables from
#'   nonmem.
#'
#' @param file A character vector of path to the files or a \code{nm_table_list}
#'   object created with \code{list_nm_tables}.
#' @param dir Location of the model files.
#' @param combined Logical value indicating whether multiple tables should be
#'   combined into a single one. If the number of rows does not match an error
#'   will be returned.
#' @param rm_duplicates Logical value indicating whether duplicated columns
#'   should be removed.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param simtab If \code{TRUE} only reads in simulation tables, if \code{FALSE}
#'   only reads estimation tables. Default \code{NULL} reads all tables.
#' @param ziptab If \code{TRUE} search for the tables that have been compressed
#'   and renamed ´<file>.zip'.
#' @param ... Additional arguments to be passed to the
#'   \code{\link[readr]{read_table2}} or \code{\link[readr]{read_csv}}
#'   functions.
#'
#' @examples
#' \dontrun{
#' # Import tables manually and return them as a list of individual tables
#' nm_tables <- read_nm_tables(file = c('sdtab001', 'patab001'),
#'                             dir = 'models', combined = FALSE)
#'
#' # Import tables manually and return them as a single merged table
#' nm_tables <- read_nm_tables(file = c('sdtab001', 'patab001'),
#'                             dir = 'models', combined = TRUE)
#'
#' # Import tables automatically (used internally by create_nm_xpdb())
#' nm_tables <- read_nm_model(file = 'run001.lst', dir = 'models') %>%
#'               list_nm_tables() %>%
#'               read_nm_tables()
#'
#' # Passing arguments to readr via `...`
#' # (e.g. import columns as character and only first 10 rows)
#' nm_tables <- read_nm_tables(file = 'sdtab001', dir = 'models',
#'                             col_type = readr::cols(.default = 'c'),
#'                             n_max = 10)
#'
#' }
#' @export
read_nm_tables <- function(file          = NULL,
                           dir           = NULL,
                           combined      = TRUE,
                           rm_duplicates = TRUE,
                           quiet         = FALSE,
                           simtab        = NULL,
                           ziptab        = TRUE,
                           ...) {
  # Check inputs
  if (is.null(file)) stop('Argument `file` required.', call. = FALSE)
  
  if (!is.null(file) && !is.table.list.nm(file)) {
    file <- tibble::tibble(problem   = 1, 
                           file      = file_path(dir, file),
                           firstonly = FALSE,
                           simtab    = FALSE)
  }
  
  user_mode <- !is.table.list.nm(file)
  
  # Filter tables if needed
  if (!is.null(simtab)) file <- file[file$simtab == simtab, ]
  msg('\nLooking for nonmem output tables.', quiet)
  
  # Check that file exists
  if (is.null(file) || !any(file.exists(file$file))) {
    stop('No table files could be found.', call. = FALSE)
  }
  
  if (any(duplicated(file$file))) {
    stop('No table imported due to duplicated names.', call. = FALSE)
  }
  
  tables <- file[file.exists(file$file), ]
  
  # Search for compressed (.zip) tables
  if (ziptab) { 
    tables_zip <- file[!file.exists(file$file), ]
    
    if (nrow(tables_zip) > 0) {
      tables_zip$file <- stringr::str_c(tables_zip$file, '.zip')
      tables_zip <- tables_zip[file.exists(tables_zip$file), ]
      
      if (nrow(tables_zip) > 0) {
        tables <- tables %>% 
          dplyr::bind_rows(tables_zip) %>% 
          dplyr::arrange_at(.vars = c('problem', 'file'))
      }
    }
  }
  
  # Save the MD5 checksum for all available files
  tables <- tables %>% 
    dplyr::mutate(md5  = tools::md5sum(!!rlang::sym('file')),
                  name = basename(!!rlang::sym('file')))
  
  # Print reading messages
  tables %>% 
    dplyr::mutate(name = stringr::str_c(
      !!rlang::sym('name'), 
      dplyr::if_else(!!rlang::sym('firstonly'), ' (firstonly)', '')
    )) %>% 
    dplyr::group_by_at(.vars = c('problem', 'simtab')) %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(string = purrr::map_chr(
      .x = !!rlang::sym('data'), 
      .f = ~stringr::str_c(.x$name, collapse = ', '))) %>% 
    {stringr::str_c(.$string, ' [$prob no.', .$problem, 
                    dplyr::if_else(.$simtab, ', simulation', ''), 
                    ']', collapse = '\n         ')} %>% 
    {msg(c('Reading: ', .), quiet)}
  
  # Save the file information
  file <- tables %>% 
    dplyr::select_at(.vars = dplyr::vars('problem', 'name', 'file', 'md5')) %>% 
    dplyr::mutate(type = 'data')
  
  # Collect options for table import
  tables <- tables %>% 
    dplyr::select(-dplyr::one_of('md5')) %>% 
    dplyr::mutate(top = purrr::map(.x = !!rlang::sym('file'), 
                                   .f = ~readr::read_lines(file = .x, n_max = 3))) %>% 
    dplyr::mutate(args = purrr::pmap(., .f = read_args, quiet, ...)) %>% 
    tidyr::unnest(!!rlang::sym('args')) %>% 
    dplyr::select(dplyr::one_of('problem', 'name', 'simtab', 
                                'firstonly', 'fun', 'params'))
  
  if (nrow(tables) == 0) stop('No table imported.', call. = FALSE)
  
  # Read in data
  tables <- tables %>% 
    dplyr::mutate(data = purrr::map2(.x = !!rlang::sym('fun'), 
                                     .y = !!rlang::sym('params'),
                                     .f = ~do.call(what = .x, args = .y)),
                  data = purrr::map(.x = !!rlang::sym('data'), 
                                    .f = ~tidyr::drop_na(.x, 1)))
  
  # Return a list of datasets if asked by the user
  if (!combined) {
    return(purrr::set_names(x = purrr::map(.x = tables$data, 
                                           .f = ~tidyr::drop_na(.x, 1)),
                            nm = tables$name))
  }
  
  # Index datasets
  tables <- tables %>% 
    dplyr::mutate(index = purrr::map2(.x = !!rlang::sym('name'), 
                                      .y = !!rlang::sym('data'), 
                                      .f = index_table),
                  nrow  = purrr::map_dbl(.x = !!rlang::sym('data'), 
                                         .f = nrow)) %>% 
    
    # Combine tables with same number of rows
    dplyr::group_by_at(.vars = c('problem', 'simtab', 'firstonly')) %>% 
    {# Temporary handling of changes in tidyr 1.0
      if (tidyr_new_interface()) {
        tidyr::nest(.data = ., out = -dplyr::one_of('problem', 'simtab', 'firstonly'))
      } else {
        tidyr::nest(.data = ., .key = 'out')
      }} %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(out = purrr::map(.x = !!rlang::sym('out'), 
                                   .f = combine_tables)) %>% 
    tidyr::unnest(!!rlang::sym('out'))
  
  if (nrow(tables) == 0) stop('No table imported.', call. = FALSE)
  
  # Remove duplicated columns to decrease xpdb size
  if (rm_duplicates) {
    tables <- tables %>% 
      dplyr::mutate(data = purrr::map2(
        .x = !!rlang::sym('data'), 
        .y = !!rlang::sym('index'),
        .f = ~dplyr::select(.x, dplyr::one_of(unique(.y$col)))))
  }
  
  # Merge firstonly tables with main tables
  if (any(tables$firstonly)) {
    msg('Consolidating tables with `firstonly`', quiet)
    tables <- tables %>%
      dplyr::group_by_at(.vars = c('problem', 'simtab')) %>% 
      {# Temporary handling of changes in tidyr 1.0
        if (tidyr_new_interface()) {
          tidyr::nest(.data = ., out = -dplyr::one_of('problem', 'simtab'))
        } else {
          tidyr::nest(.data = ., .key = 'out')
        }} %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(out = purrr::map(.x = !!rlang::sym('out'), 
                                     .f = merge_firstonly, quiet)) %>% 
      tidyr::unnest(!!rlang::sym('out'))
  }
  
  if (nrow(tables) == 0) stop('No table imported.', call. = FALSE)
  
  # Convert catcov, id, occ, dvid to factor
  tables <- tables %>% 
    dplyr::mutate(data = purrr::map2(
      .x = !!rlang::sym('data'),
      .y = !!rlang::sym('index'),
      .f = ~dplyr::mutate_if(
        .tbl = .x, 
        .predicate = colnames(.x) %in% .y$col[.y$type %in% c('catcov', 'id', 'occ', 'dvid')],
        .funs = as.factor))) %>% 
    dplyr::mutate(md5_ref = purrr::map_chr(.x = !!rlang::sym('data'), 
                                           .f = digest::digest)) %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'index', 'data', 'md5_ref'))
  
  # If user mode return simple tibble as only 1 problem should be used
  if (user_mode) return(tables$data[[1]])
  
  # Make sure that the file info reflects only the files present in the index
  tables %>% 
    dplyr::select(dplyr::one_of('problem', 'index')) %>% 
    tidyr::unnest(!!rlang::sym('index')) %>% 
    dplyr::distinct(!!!rlang::syms(c('problem', 'table')), 
                    .keep_all = FALSE) %>% 
    dplyr::rename(name = dplyr::one_of('table')) %>% 
    {dplyr::semi_join(x = file,
                      y = .,
                      by = c('problem', 'name'))} %>% 
    
    # Final output
    {list(data      = tables, 
          file_info = .)} %>% 
    structure(class = 'nm_tables')
}


#' Define data import functions
#' 
#' @param fun Abbreviated `readr` data import function. 
#' Can be `csv`, `csv2` or `table`.
#' 
#' @return A data import function.
#' 
#' @keywords internal
#' @export
read_funs <- function(fun) {
  c(csv   = readr::read_csv,
    csv2  = readr::read_csv2,
    table = readr::read_table2)[fun]
}


#' Define data import arguments
#' 
#' @param top A list containing a the 3 first records of a 
#' dataset.
#' @param file The full file name of a dataset.
#' @param name The base name of the file.
#' @param quiet Should messages be displayed to the console.
#' @param col_types Defines the type of each column to be passed to 
#' the `readr` import function.
#' @param na Character string defining the values to be treated as `NA`.
#' @param comment Character string defining the value to mark comments.
#' @param skip Number of rows to be skipped before reading the data.
#' @param ... Additional arguments to be passed to the `readr` function
#' 
#' @return A list of 2 levels fun (the import function) and params (a list 
#' of arguments to be used when calling fun).
#' 
#' @keywords internal
#' @export
read_args <- function(top, file, name, quiet, col_types = readr::cols(.default = 'd'), 
                      na = 'NA', comment = 'TABLE', skip = 1, ...) {
  
  # Check the data format (i.e. 0.000 or 0.000E-00)
  if (is.na(top[3]) || !stringr::str_detect(top[3], '\\d+(\\.|,)\\d+(E[+-]\\d+)?\\s*')) {
    warning(c('Dropped: ', name, ' due to unexpected data format'), call. = FALSE)
    return(tibble::tibble(fun = list(), params = list()))
  }
  
  # Determine the proper reading function to use
  fun <- dplyr::case_when(stringr::str_detect(top[3], '\\d+,\\d+(E[+-]\\d+)?\\s*;') ~ 'csv2',
                          stringr::str_detect(top[3], '\\d+\\.\\d+(E[+-]\\d+)?\\s*,') ~ 'csv', 
                          TRUE ~ 'table')
  
  # Check whether there a TABLE NO. to be dropped
  skip_h <- dplyr::if_else(stringr::str_detect(top[1], 'TABLE NO\\.\\s+\\d'), 1, 0)
  
  # Check the headers
  ## i.e. checks for at least for 2 contiguous letters within the entire header row
  if (!stringr::str_detect(top[1 + skip_h], '[A-z]{2,}')) {
    warning(c('Dropped: ', name, ' due to missing headers.'), call. = FALSE)
    return(tibble::tibble(fun = list(), params = list()))
  }
  
  # Parse headers
  col_names <- top[1 + skip_h] %>% 
    stringr::str_trim(side = 'both') %>% 
    stringr::str_split(pattern = dplyr::case_when(fun == 'csv' ~ ',', 
                                                  fun == 'csv2' ~ ';',
                                                  fun == 'table' ~ '\\s+')) %>% 
    purrr::flatten_chr() %>% 
    stringr::str_trim()
  
  # Cleanup the elipsis
  extras <- list(...) %>% 
    purrr::discard(names(.) %in% c('problem', 'firstonly', 'simtab'))
  
  # Return the reading arguments
  tibble::tibble(fun = read_funs(fun),
                 params = list(c(list(file = file, skip = skip, comment = comment, 
                                      na = c(na, col_names), col_names = col_names,
                                      col_types = col_types), extras)))
}


#' Combine tables
#' 
#' @param x A list containing the tables (`x$data`) to be 
#' combined, their names (`x$name`), their number of rows (`x$nrow`) 
#' and their respective index (`x$index`).
#' 
#' @return A list containing `data` and `index` of the combined table.
#' 
#' @keywords internal
#' @export
combine_tables <- function(x) {
  # Check for matching length
  if (length(unique(x$nrow)) > 1) {
    warning(c('Dropped ', stringr::str_c('`', x$name, '`', collapse = ', '), 
              ' due to missmatch in row number.'), call. = FALSE)
    return(tibble::tibble(data = list(), index = list()))
    
  }
  
  # Combine tables
  tibble::tibble(data = x$data %>%
                   dplyr::bind_cols() %>%
                   purrr::set_names(make.unique(names(.))) %>%
                   tidyr::drop_na(1) %>%
                   list(),
                 index = list(dplyr::bind_rows(x$index)))
}


#' Merge firstonly table with full length tables
#' 
#' @param x A list containing the tables (`x$data`) to be 
#' merged, the firstonly flag (`x$firstonly`) and the 
#' indexes (`x$index`).
#' @param quiet Should messages be displayed to the console.
#' 
#' @return A list containing `data` and `index` of the merged table.
#' 
#' @keywords internal
#' @export
merge_firstonly <- function(x, quiet) {
  if (nrow(x) == 1) {
    # No merge needed
    return(tibble::tibble(data = x$data, index = x$index))
  } else if (nrow(x) != 2) {
    warning(c(' * Something went wrong while consolidating: ', 
              stringr::str_c(x[x$firstonly == TRUE, ]$index[[1]]$tables, 
                             collapse = ', ')), call. = FALSE) 
    return(tibble::tibble(data = list(), index = list()))
  }
  xdata   <- x$data[x$firstonly == FALSE][[1]]
  ydata   <- x$data[x$firstonly == TRUE][[1]]
  by_vars <- intersect(colnames(xdata), colnames(ydata))
  msg(c(' * Joining by: ', stringr::str_c(by_vars, collapse = ', ')), quiet)
  tibble::tibble(data = list(dplyr::left_join(x  = xdata, 
                                              y  = ydata,
                                              by = by_vars)),
                 index = x$index %>% 
                   dplyr::bind_rows() %>% 
                   list())
}


#' Index table columns
#' 
#' @param name the name of the dataset to be indexed 
#' @param data the data associated with the dataset specified in `name`.
#' 
#' @return A tibble of the index.
#' 
#' @keywords internal
#' @export
index_table <- function(name, data) {
  tab_type <- dplyr::case_when(
    stringr::str_detect(name, 'patab') ~ 'param',   # model parameters
    stringr::str_detect(name, 'catab') ~ 'catcov',  # categorical covariate
    stringr::str_detect(name, 'cotab') ~ 'contcov', # continuous covariate
    TRUE ~ 'na')
  
  data %>% 
    colnames() %>% 
    tibble::tibble(table = name,
                   col   = ., 
                   type  = NA_character_, 
                   label = NA_character_,     # Feature to be added in future releases
                   units = NA_character_) %>% # Feature to be added in future releases
    dplyr::mutate(type = dplyr::case_when(
      .$col == 'ID'    ~ 'id',
      .$col == 'DV'    ~ 'dv',
      .$col == 'TIME'  ~ 'idv',
      .$col == 'OCC'   ~ 'occ',
      .$col == 'DVID'  ~ 'dvid',
      .$col == 'AMT'   ~ 'amt',
      .$col == 'MDV'   ~ 'mdv',
      .$col == 'EVID'  ~ 'evid',
      .$col == 'IPRED' ~ 'ipred',
      .$col == 'PRED'  ~ 'pred',
      .$col %in% c('RES', 'WRES', 'CWRES', 'IWRES', 'EWRES', 'NPDE') ~ 'res',
      stringr::str_detect(.$col, 'ETA\\d+|ET\\d+') ~ 'eta',
      stringr::str_detect(.$col, '^A\\d+$') ~ 'a',
      TRUE ~ tab_type))
}
