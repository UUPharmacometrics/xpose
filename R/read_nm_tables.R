#' NONMEM output table import function
#'
#' @description Quickly import NONMEM output tables into R. This function automatically 
#' detects the optimal settings to import the tables from nonmem.
#'
#' @param file A character vector of path to the files or a \code{nm_table_list} object created with \code{list_nm_tables}.
#' @param dir Location of the model files.
#' @param combined Logical value indicating whether multiple tables should be combined into a single one. If the number of rows 
#' does not match an error will be returned.
#' @param rm_duplicates Logical value indicating whether duplicated columns should be removed.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param simtab If \code{TRUE} only reads in simulation tables, if \code{FALSE} only reads estimation tables. 
#' Default \code{NULL} reads all tables.
#' @param ziptab If \code{TRUE} search for the tables that have been compressed and renamed ´<file>.zip'.
#' @param ... Additional arguments to be passed to the \code{\link[readr]{read_table}} or \code{\link[readr]{read_csv}} functions.
#' 
#' @section Table format requirement:
#' When using \code{read_nm_tables} with the \code{combined} argument set to \code{FALSE} an \code{ID} column 
#' must be present in all data tables. When \code{combined} is set to \code{TRUE} instead an \code{ID} column must be 
#' present in at least one table for each problem and for each `firstonly` category. \code{ID} columns are required 
#' to properly combine/merge tables and removing \code{NA} records. If the \code{ID} column is missing from a table and 
#' \code{combined = FALSE} \code{read_nm_tables} will return the following warning: \code{Unknown variables: `ID`}. While
#' the data is returned beware that \code{NA} records might be left in the data and the output should be checked carefully.
#' If \code{combined = TRUE} \code{read_nm_tables} xpose is more strict and will return the following warning instead: 
#' \code{Dropped `<tablenames>` due to missing required `ID` column.}.
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
#' # Import tables automatically (used internally by xpose_data())
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
  
  if (!is.null(file) && !is.nm.table.list(file)) {
    file <- dplyr::tibble(problem   = 1, 
                          file      = file_path(dir, file),
                          firstonly = FALSE,
                          simtab    = FALSE)
  }
  
  user_mode <- !is.nm.table.list(file)
  
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
  
  # Check for readr lazy loading
  if (readr::should_read_lazy() & .Platform$OS.type == "windows") {
    warning("Using lazy loading in `readr` on Windows can cause unexpected behavior and is not recommended for `xpose`.", call. = FALSE)
  }
  
  tables <- file[file.exists(file$file), ]
  
  # Search for compressed tables
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
  
  # Print reading messages
  tables %>% 
    dplyr::mutate(grouping = 1:n(),
                  name = stringr::str_c(basename(.$file), dplyr::if_else(.$firstonly, ' (firstonly)', ''))) %>% 
    dplyr::group_by_at(.vars = c('problem', 'simtab')) %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(string = purrr::map_chr(.$data, ~stringr::str_c(.$name, collapse = ', '))) %>% 
    {stringr::str_c(.$string, ' [$prob no.', .$problem, dplyr::if_else(.$simtab, ', simulation', ''), 
                    ']', collapse = '\n         ')} %>% 
    {msg(c('Reading: ', .), quiet)}
  
  # Collect options for table import
  tables <- tables %>% 
    dplyr::mutate(top = purrr::map(.$file, ~readr::read_lines(file = ., n_max = 3)),
                  grouping = 1:n()) %>% 
    dplyr::group_by_at(.vars = 'grouping') %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(args = purrr::map(.x = .$data, .f = read_args, quiet, ...)) %>% 
    tidyr::unnest(dplyr::one_of('data')) %>% 
    tidyr::unnest(dplyr::one_of('args')) %>% 
    dplyr::mutate(name = basename(.$file)) %>% 
    dplyr::select(dplyr::one_of('problem', 'name', 'simtab', 'firstonly', 'fun', 'params'))
  
  if (nrow(tables) == 0) stop('No table imported.', call. = FALSE)
  
  # Read in data
  tables <- tables %>% 
    dplyr::bind_cols(tables %>% 
                       dplyr::select(dplyr::one_of(c('fun', 'params'))) %>% 
                       dplyr::mutate(table = purrr::map2(.x = .$fun, .y = .$params, .f = do.call)) %>% 
                       dplyr::pull(table) %>% 
                       dplyr::tibble(data = .))
  
  if (!combined) {
    return(purrr::set_names(x = purrr::map(tables$data, ~tidyr::drop_na(., dplyr::one_of('ID'))),
                            nm = tables$name))
  }
  
  # Index datasets
  tables <- tables %>% 
    dplyr::mutate(grouping = 1:n()) %>% 
    dplyr::group_by_at(.vars = 'grouping')
  
  ## TEMP handling
  if (tidyr_new_interface()) {
    tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('grouping'))
  } else {
    tables <- tables %>% tidyr::nest(.key = 'tmp')
  }
  ## END TEMP
  
  tables <- tables %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(index = purrr::map(.$tmp, index_table),
                  nrow =  purrr::map_dbl(.$tmp, ~nrow(.$data[[1]]))) %>% 
    tidyr::unnest(dplyr::one_of('tmp')) %>% 
    dplyr::ungroup()
  
  
  # Combine tables with same number of rows
  tables <- tables %>% 
    dplyr::group_by_at(.vars = c('problem', 'simtab', 'firstonly'))
  
  ## TEMP handling
  if (tidyr_new_interface()) {
    tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('problem', 'simtab', 'firstonly'))
  } else {
    tables <- tables %>% tidyr::nest(.key = 'tmp')
  }
  ## END TEMP
  
  tables <- tables %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(out = purrr::map(.$tmp, combine_tables)) %>% 
    tidyr::unnest(dplyr::one_of('out')) %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'firstonly', 'data', 'index'))
  
  if (nrow(tables) == 0) stop('No table imported.', call. = FALSE)
  
  # Remove duplicated columns to decrease xpdb size
  if (rm_duplicates) {
    tables <- tables %>% 
      dplyr::mutate(grouping = 1:n()) %>% 
      dplyr::group_by_at(.vars = 'grouping')
    
    ## TEMP handling
    if (tidyr_new_interface()) {
      tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('grouping'))
    } else {
      tables <- tables %>% tidyr::nest(.key = 'tmp')
    }
    ## END TEMP
    
    tables <- tables %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(out = purrr::map(.$tmp, ~dplyr::select(.$data[[1]], 
                                                           dplyr::one_of(unique(unlist(.$index[[1]]$col)))))) %>% 
      tidyr::unnest(dplyr::one_of('tmp')) %>% 
      dplyr::select(dplyr::one_of('problem', 'simtab', 'firstonly', 'index', 'out')) %>% 
      dplyr::rename(!!rlang::sym('data') := dplyr::one_of('out'))
  }
  
  # Merge firsonly tables with main tables
  if (any(tables$firstonly)) {
    msg('Consolidating tables with `firstonly`', quiet)
    tables <- tables %>%
      dplyr::group_by_at(.vars = c('problem', 'simtab'))
    
    ## TEMP handling
    if (tidyr_new_interface()) {
      tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('problem', 'simtab'))
    } else {
      tables <- tables %>% tidyr::nest(.key = 'tmp')
    }
    ## END TEMP
    
    tables <- tables %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(out = purrr::map(.$tmp, merge_firstonly, quiet)) %>% 
      tidyr::unnest(dplyr::one_of('out')) %>% 
      dplyr::select(dplyr::one_of('problem', 'simtab', 'data', 'index'))
  }
  
  if (nrow(tables) == 0) stop('No table imported.', call. = FALSE)
  
  # Convert catcov, id, occ, dvid to factor
  tables <- tables %>% 
    dplyr::mutate(grouping = .$problem) %>% 
    dplyr::group_by_at(.vars = 'grouping')
  
  ## TEMP handling
  if (tidyr_new_interface()) {
    tables <- tables %>% tidyr::nest(tmp = -dplyr::one_of('grouping'))
  } else {
    tables <- tables %>% tidyr::nest(.key = 'tmp')
  }
  ## END TEMP
  
  tables <- tables %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(tmp = purrr::map(.$tmp, function(x) {
      col_to_factor <- colnames(x$data[[1]]) %in% 
        x$index[[1]]$col[x$index[[1]]$type %in% c('catcov', 'id', 'occ', 'dvid')]
      x$data[[1]] <- dplyr::mutate_if(x$data[[1]], col_to_factor, as.factor)
      x
    })) %>% 
    tidyr::unnest(dplyr::one_of('tmp')) %>% 
    dplyr::mutate(modified = FALSE) %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'index', 'data', 'modified'))
  
  # If user mode return simple tibble as only 1 problem should be used
  if (user_mode) return(tables$data[[1]])
  tables
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
  if (utils::packageVersion("readr") > "1.4.0") {
  c(csv   = readr::read_csv,
    csv2  = readr::read_csv2,
    table = readr::read_table)[fun]
  } else {
    c(csv   = readr::read_csv,
      csv2  = readr::read_csv2,
      table = readr::read_table2)[fun]
  }
}


#' Define data import arguments
#' 
#' @param x A list containing a the 3 first records of a 
#' dataset under `[[1]]`.
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
read_args <- function(x, quiet, col_types = readr::cols(.default = 'd'), 
                      na = 'NA', comment = 'TABLE', skip = 1, ...) {
  
  top <- x$top[[1]]
  
  if (is.na(top[3]) || !stringr::str_detect(top[3], '\\d+E[+-]\\d+\\s*')) {
    warning(c('Dropped: ', basename(x$file), ' due to unexpected data format'), call. = FALSE)
    return(dplyr::tibble(fun = list(), params = list()))
  }
  
  fun <- dplyr::case_when(stringr::str_detect(top[3], '\\d,\\d+E[+-]\\d+\\s*;') ~ 'csv2',
                          stringr::str_detect(top[3], '\\d.\\d+E[+-]\\d+\\s*,') ~ 'csv', 
                          TRUE ~ 'table')
  
  skip_h <- dplyr::if_else(stringr::str_detect(top[1], 'TABLE NO\\.\\s+\\d'), 1, 0)
  
  if (!stringr::str_detect(top[1 + skip_h], '[A-z]{2,}+')) {
    warning(c('Dropped: ', basename(x$file), ' due to missing headers.'), call. = FALSE)
    return(dplyr::tibble(fun = list(), params = list()))
  }
  
  col_names <- top[1 + skip_h] %>% 
    stringr::str_trim(side = 'both') %>% 
    stringr::str_split(pattern = dplyr::case_when(fun == 'csv' ~ ',', 
                                                  fun == 'csv2' ~ ';',
                                                  fun == 'table' ~ '\\s+')) %>% 
    purrr::flatten_chr() %>% 
    stringr::str_trim()
  
  dplyr::tibble(fun = read_funs(fun),
                params = list(list(file = x$file, skip = skip, comment = comment, 
                                   na = c(na, col_names), col_names = col_names, 
                                   col_types = col_types, ...)))
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
    return(dplyr::tibble(data = list(), index = list()))
    
  }
  
  # Check for ID column
  if (!any(purrr::map_lgl(x$index, ~any(.$type == 'id')))) {
    warning(c('Dropped ', stringr::str_c('`', x$name, '`', collapse = ', '), 
              ' due to missing required `ID` column.'), call. = FALSE)
    return(dplyr::tibble(data = list(), index = list()))
  }
  
  # Prepare the combined data
  # Note: here the data may contain duplicated columns so 
  #       we de-duplicate them in base R to avoid errors 
  #       with the tidyverse.
  tmp_df <- do.call("cbind", unname(x$data))
  tmp_df <- tmp_df[, which(!duplicated(names(tmp_df))) ] %>% 
    tibble::as_tibble() %>% 
    tidyr::drop_na(dplyr::one_of('ID')) %>%
    list()
  
  # Combine tables
  dplyr::tibble(data  = tmp_df,
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
    return(dplyr::tibble(data = x$data, index = x$index))
  } else if (nrow(x) != 2) {
    warning(c(' * Something went wrong while consolidating: ', 
              stringr::str_c(x[x$firstonly == TRUE, ]$index[[1]]$tables, 
                             collapse = ', ')), call. = FALSE) 
    return(dplyr::tibble(data = list(), index = list()))
  }
  xdata   <- x$data[x$firstonly == FALSE][[1]]
  ydata   <- x$data[x$firstonly == TRUE][[1]]
  by_vars <- intersect(colnames(xdata), colnames(ydata))
  msg(c(' * Joining by: ', stringr::str_c(by_vars, collapse = ', ')), quiet)
  dplyr::tibble(data = list(dplyr::left_join(x  = xdata, 
                                             y  = ydata,
                                             by = by_vars)),
                index = x$index %>% 
                  dplyr::bind_rows() %>% 
                  list())
}


#' Index table columns
#' 
#' @param x A list containing the tables (`x$data`) to be 
#' combined along with their respective names (`x$name`).
#' 
#' @return A tibble of the index.
#' 
#' @keywords internal
#' @export
index_table <- function(x) { 
  tab_type <- dplyr::case_when(
    stringr::str_detect(tolower(x$name), 'patab') ~ 'param',   # model parameters
    stringr::str_detect(tolower(x$name), 'catab') ~ 'catcov',  # categorical covariate
    stringr::str_detect(tolower(x$name), 'cotab') ~ 'contcov', # continuous covariate
    TRUE ~ 'na')
  
  tad.var <- c('TSPD', 'TSLD', 'TAD', 'TPD')
  tad.var <- tad.var[ tad.var %in% (x$data[[1]] %>% colnames()) ][1]
  
  x$data[[1]] %>% 
    colnames() %>% 
    dplyr::tibble(table = x$name,
                  col   = ., 
                  type  = NA_character_, 
                  label = NA_character_,     # Feature to be added in future releases
                  units = NA_character_) %>% # Feature to be added in future releases
    dplyr::mutate(type = dplyr::case_when(
      .$col == 'ID' ~ 'id',
      .$col == 'DV' ~ 'dv',
      .$col == 'TIME' ~ 'idv',
      .$col == tad.var ~ 'tad',
      .$col == 'OCC' ~ 'occ',
      .$col == 'DVID' ~ 'dvid',
      .$col == 'AMT' ~ 'amt',
      .$col == 'MDV' ~ 'mdv',
      .$col == 'EVID' ~ 'evid',
      .$col == 'IPRED' ~ 'ipred',
      .$col == 'PRED' ~ 'pred',
      .$col %in% c('RES', 'WRES', 'CWRES', 'IWRES', 'EWRES', 'NPDE') ~ 'res',
      stringr::str_detect(.$col, 'ETA\\d+|ET\\d+') ~ 'eta',
      stringr::str_detect(.$col, '^A\\d+$') ~ 'a',
      TRUE ~ tab_type))
  
}
