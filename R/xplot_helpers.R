#' Check .problem, .subprob and .method
#' 
#' @param xpdb An xpose database object.
#' @param .problem The problem to be checked.
#' @param .subprob The subproblem to be checked.
#' @param .method The estimation method to be checked.
#' 
#' @keywords internal
#' @export
check_problem <- function(.problem, .subprob, .method) {
  bad_input <- list(.problem = .problem, .subprob = .subprob, .method = .method) %>% 
    purrr::keep(.p = ~length(.x) > 1) %>% 
    names()
  
  if (length(bad_input) > 0) {
    bad_input %>% 
      stringr::str_c('`', ., '`', collapse = ', ') %>% 
      {stop('Argument', ., ' must be of length 1.', call. = FALSE)}
  }
}


#' Check xpdb
#' 
#' @param xpdb An xpose database object.
#' @param check The `xpdb` slot to be checked, can be `data`, `files`, 
#' `summary`, `special`, `code`, a file name, or `FALSE`. IF `FALSE` the slot 
#' test will be skipped.
#' 
#' @return Silent if successful check, else returns error.
#' 
#' @keywords internal
#' @export
check_xpdb <- function(xpdb, check = 'data') {
  # Check the class
  if (!is.xpdb(xpdb)) {
    stop('Bad input to the argument `xpdb`', call. = FALSE)
  }
  
  skip  <- ifelse(check == FALSE, TRUE, FALSE)
  check <- ifelse(!check %in% c('data', 'files', 'summary', 
                                'special', 'code', 'file_info'), 
                  'files', check)
  
  # Check for the presence of data
  if (!skip && is.null(xpdb[[check]])) {
    stop('No `', check, '` slot could be found in this xpdb.', call. = FALSE)
  }
}

#' Check plot `type`
#' 
#' @param type A character string guiding the plot type.
#' @param allowed A character vector of all allowed types e.g. `c('p', 'l')`.
#' 
#' @return Silent if proper user input else warns for unrecognized `type`.
#' 
#' @keywords internal
#' @export
check_plot_type <- function(type, allowed) {
  type  <- stringr::str_extract_all(type, pattern = '.')[[1]]
  not_allowed <- type[!type %in% allowed]
  if (length(not_allowed) > 0) {
    warning('Plot type ', stringr::str_c('"',not_allowed, '"', collapse = ', '), 
            ' not recognized.', call. = FALSE)
  }
}


#' Check plot scales
#' 
#' @param scale The axis to be checked. Can be `x` or `y`.
#' @param log A string to guide which axis (e.g. `x`, `y` or `xy`) should be 
#' plotted on log scale.
#' 
#' @return A string taking `continuous` or `log10` as value.
#' 
#' @keywords internal
#' @export
check_scales <- function(scale, log) {
  if (is.null(log)) return('continuous')
  ifelse(stringr::str_detect(string = log, pattern = scale), 'log10', 'continuous')
}


#' Append suffix contained in the `xp_theme` to titles
#' 
#' @param xpdb An xpose database object.
#' @param string A string to which the suffix will be appended.
#' @param type A string determining what suffix type should be used 
#' in the `xp_theme`. Can be one of `title`, `subtitle`, `caption` or `tag`.
#' 
#' @return The modified `string`.
#' 
#' @keywords internal
#' @export
append_suffix <- function(xpdb, string = NULL, type = NULL) {
  if (is.null(string)) return()
  stringr::str_c(string, xpdb$xp_theme[stringr::str_c(type, '_suffix')], sep = '')
}


#' Parse keywords in string based on values contained in an xpdb
#' 
#' @param string A string containing keywords (e.g. `@nobs`) to be parsed 
#' (e.g. title, label, etc.) using values stored in the `xpdb$summary`.
#' @param xpdb An xpose database object.
#' @param problem The $problem number to be used.
#' @param quiet Should messages be displayed to the console.
#' @param extra_key A vector of additional keywords not available in the `xpdb$summary`.
#' @param extra_value A vector of values matching the order of `extra_key`.
#' @param ignore_key A vector of keywords to be ignored i.e. warnings will not be returned.
#' 
#' @return The parsed `string`.
#' 
#' @keywords internal
#' @export
parse_title <- function(string, xpdb, problem, quiet, extra_key = NULL, 
                        extra_value = NULL, ignore_key = NULL) {
  # Extract keywords from the string
  keyword <- string %>% 
    stringr::str_extract_all('@[[:alnum:]]+') %>% 
    purrr::flatten_chr() %>% 
    stringr::str_replace(stringr::fixed('@'), '') %>% 
    subset(!. %in% ignore_key)
  
  # Get the associated values in the summart
  values <- xpdb$summary[xpdb$summary$problem %in% c(0, problem) & 
                           xpdb$summary$label %in% keyword, ]
  values <- values[!duplicated(values$label, fromLast = TRUE), ]
  
  # If needed add extra values e.g. in xpose_save
  if (!is.null(extra_key) && any(extra_key %in% keyword)) {
    values <- dplyr::bind_rows(values,
                               tibble::tibble(problem = 0,
                                              subp  = 0,
                                              label = extra_key,
                                              value = extra_value))
  }
  
  # Remove unmatched keywords from the list
  if (!all(keyword %in% values$label)) {
    keyword[!keyword %in% values$label & !keyword %in% ignore_key] %>% 
      unique() %>% 
      stringr::str_c(collapse = ', ') %>%   
      {warning(c(., ' is not part of the available keywords. Check ?template_titles for a full list.'), call. = FALSE)}
    keyword <- keyword[keyword %in% values$label]
  }
  
  if (length(keyword) == 0) return(string)
  
  # Replaces values in the string
  string %>% 
    stringr::str_replace_all(stringr::str_c('@', keyword, collapse = '|'), 
                             '${values$value[values$label == \"\\0\"]}') %>% 
    stringr::str_replace_all('\\"@', '\\"') %>% 
    stringr::str_interp()
}


#' Subset an `xp_theme`
#' 
#' @param xp_theme An xpose theme list.
#' @param regex A string used to find matching names in the `xp_theme`
#' @param action A string used to determine the action of on the xp_theme.
#' `keep` will subset the `xp_theme` to only the names matching the `regex`, 
#' while `drop` will drop the matches.
#' 
#' @return A modified `xp_theme`
#' 
#' @keywords internal
#' @export
filter_xp_theme <- function(xp_theme, regex = NULL, action = 'keep') {
  match <- stringr::str_detect(names(xp_theme), regex)
  if (action == 'drop') match <- !match
  xp_theme[match]
}


#' Get all data problems
#' 
#' @param xpdb An xpose database object.
#' 
#' @return The number of the all data problems as an integer 
#' vector.
#' 
#' @keywords internal
#' @export
all_data_problem <- function(xpdb) {
  prob_n <- xpdb$data$problem
  if (length(prob_n) == 0) return(NA_integer_)
  unique(prob_n)
}


#' Get last data problem
#'
#' @description Used by several internal functions when the problem argument has not 
#' been supplied by the user.
#' 
#' @param xpdb An xpose database object.
#' @param simtab Logical if `TRUE` the last simulation problem is returned. If `FALSE` 
#' the last estimation problem is returned.
#' 
#' @return The last estimation problem when `simtab = FALSE`, 
#' else the last simulation problem as an integer.
#' 
#' @keywords internal
#' @export
last_data_problem <- function(xpdb, simtab = FALSE) {
  prob_n <- xpdb$data$simtab
  if (!simtab) prob_n <- !prob_n
  prob_n <- xpdb$data$problem[prob_n]
  if (length(prob_n) == 0) return(NA_integer_)
  max(prob_n)
}


#' Get the default data problem to be plotted 
#' 
#' @description Used by plot functions when the problem argument has not 
#' been supplied by the user.
#' 
#' @param xpdb An xpose database object.
#' 
#' @return The last estimation problem when at least one estimation problem is present, 
#' else the last simulation problem as an integer.
#' 
#' @keywords internal
#' @export
default_plot_problem <- function(xpdb){
  last_data_problem(xpdb, simtab = all(xpdb$data$simtab))
}


#' Get all file problems
#' 
#' @param xpdb An xpose database object.
#' @param ext The file extension to be used.
#' 
#' @return The number of the all file problems as an integer 
#' vector for the given file `ext`.
#' 
#' @keywords internal
#' @export
all_file_problem <- function(xpdb, ext) {
  prob_n <- xpdb$files$problem[xpdb$files$extension %in% ext]
  prob_n <- unique(prob_n)
  if (length(prob_n) == 0) return(NA_integer_)
  prob_n
}

#' Get last file problem
#' 
#' @param xpdb An xpose database object.
#' @param ext The file extension to be used.
#' 
#' @return The number of the last file problem as an integer 
#' for the given file `ext`.
#' 
#' @keywords internal
#' @export
last_file_problem <- function(xpdb, ext) {
  prob_n <- all_file_problem(xpdb = xpdb, ext = ext)
  max(prob_n)
}

#' Get last file subproblem
#' 
#' @param xpdb An xpose database object.
#' @param ext The file extension to be used.
#' @param .problem The $problem number to be used.
#' 
#' @return The number of the last file subproblem as an integer 
#' for the given `.problem` and file `ext`.
#' 
#' @keywords internal
#' @export
last_file_subprob <- function(xpdb, ext, .problem) {
  subprob_n <- xpdb$files$subprob[xpdb$files$extension %in% ext & xpdb$files$problem %in% .problem]
  subprob_n <- unique(subprob_n)
  if (length(subprob_n) == 0) return(NA_integer_)
  max(subprob_n)
}


#' Get last file estimation method
#' 
#' @param xpdb An xpose database object.
#' @param ext The file extension to be used.
#' @param .problem The $problem number to be used.
#' @param .subprob The subproblem number to be used.
#' 
#' @return The number of the last file method as character 
#' for the given `.problem` and file `ext`.
#' 
#' @keywords internal
#' @export
last_file_method <- function(xpdb, ext, .problem, .subprob) {
  method_n <- xpdb$files$method[xpdb$files$extension %in% ext & 
                                  xpdb$files$problem %in% .problem & 
                                  xpdb$files$subprob %in% .subprob]
  method_n <- unique(method_n)
  if (length(method_n) == 0) return(NA_integer_)
  method_n[length(method_n)]
}

#' Return names of columns having several unique values
#' 
#' @param xpdb An xpose database object.
#' @param .problem The $problem number to be used.
#' @param cols A vector of column names to be checked.
#' @param quiet Should messages be displayed to the console.
#' 
#' @return A subset of `col` for which more than one single 
#' value was found in the data.
#' 
#' @keywords internal
#' @export
drop_fixed_cols <- function(xpdb, .problem, cols, quiet) {
  if (is.null(cols)) return()
  
  # Get the column names to be removed
  cols_rm <- get_data(xpdb, .problem = .problem) %>% 
    dplyr::select_at(cols) %>%
    dplyr::select_if(.predicate = function(x) length(unique(x)) == 1) %>% 
    colnames()
  if (length(cols_rm) == 0) return(cols)
  
  # Get the column names to be kept
  cols <- dplyr::setdiff(x = cols, y = cols_rm)
  if (length(cols) == 0) {
    stop('No non-fixed variables available for plotting.', call. = FALSE)
  }
  
  # Print message
  dplyr::if_else(length(cols_rm) > 5, 
                 stringr::str_c(stringr::str_c(cols_rm[1:5], collapse = ', '), 
                                '... and', length(cols_rm) - 5 , 'more', sep = ' '),
                 stringr::str_c(cols_rm , collapse = ', ')) %>%
    {msg(c('Dropped fixed variables ', .,'.'), quiet)}
  
  cols
}


#' Access xpdb index information for a given variable or variable type
#' 
#' @param xpdb An xpose database object.
#' @param .problem The $problem number to be used.
#' @param col The column name to be searched in the index. Alternative to arg `type`.
#' @param type The type of column to searched in the index. Alternative to `col`.
#' @param silent Should the function be silent or return errors.
#' 
#' @return A subset of the xpdb index as tibble with columns: 
#' col (column name), type (column type in the index), 
#' label (label associated with the column) and units 
#' (units associated with the column)
#' 
#' @keywords internal
#' @export
xp_var <- function(xpdb, .problem, col = NULL, type = NULL, silent = FALSE) {
  if (!all(.problem %in% xpdb$data$problem)) {
    stop('$prob no.', stringr::str_c(.problem[!.problem %in% xpdb$data$problem], collapse = ', '), 
         ' not found in model output data.', call. = FALSE)
  }
  
  index <- xpdb$data[xpdb$data$problem == .problem, ]$index[[1]]
  if (!is.null(type)) {
    index <- index[index$type %in% type, ] 
  } else {
    index <- index[index$col %in% col, ]
  }
  
  if (nrow(index) == 0) {
    if (silent) return()
    stop('Column ', 
         ifelse(!is.null(type), 
                stringr::str_c('type ', stringr::str_c('`',type, '`', collapse = ', ')),
                stringr::str_c('`',col, '`', collapse = ', ')),
         ' not available in data for problem no.', .problem, 
         '. Check `list_vars()` for an exhaustive list of available columns.', 
         call. = FALSE)
  }
  
  index %>% 
    dplyr::distinct(!!rlang::sym('col'), .keep_all = TRUE) %>% 
    dplyr::select(dplyr::one_of('col', 'type', 'label', 'units')) %>% 
    dplyr::arrange_at(.vars = c('type', 'col'))
}


#' Set new default value for ggplot2 aesthetics
#' 
#' @param fun_aes Default ggplot2 aesthetics mapping defined for the function
#' @param user_aes ggplot2 aesthetics mapping inputted by the user. These 
#' aesthetics will overwrite matching elements in `fun_aes`.
#' 
#' @return ggplot2 aesthetics mapping
#' 
#' @keywords internal
#' @export
aes_c <- function(fun_aes, user_aes) {
  if (is.null(user_aes)) return(fun_aes)
  aes <- c(fun_aes[!names(fun_aes) %in% names(user_aes)], user_aes)
  structure(aes, class = 'uneval')
}


#' Convenience function to easily filter ggplot2 aesthetics
#' 
#' @param mapping ggplot2 aesthetics
#' @param drop names of ggplot2 aesthetics to be removed from the `mapping`. 
#' Alternative to the `keep_only` argument.
#' @param keep_only names of the only ggplot2 aesthetics to be kept in the 
#' `mapping`. Alternative to the `drop` argument.
#' 
#' @return ggplot2 aesthetics mapping
#' 
#' @keywords internal
#' @export
aes_filter <- function(mapping, drop = NULL, keep_only = NULL) {
  if (is.null(mapping)) return()
  if (!is.null(keep_only)) {
    aes <- mapping[names(mapping) %in% keep_only]
  } else {
    aes <- mapping[!names(mapping) %in% drop]
  }
  if (length(aes) == 0) return()
  aes
}


#' Convenience function to easily rename ggplot2 aesthetics
#' 
#' @param mapping ggplot2 aesthetics
#' @param from names of ggplot2 aesthetics to be renamed. Note: should match the 
#' order of arg `to`.
#' @param to new names to be set for ggplot2 aesthetics. Note: should match the 
#' order of arg `from`.
#' 
#' @return ggplot2 aesthetics mapping
#' 
#' @keywords internal
#' @export
aes_rename <- function(mapping, from, to) {
  if (is.null(mapping)) return()
  if (!any(from %in% names(mapping))) return(mapping)
  names(mapping)[match(from, names(mapping))] <- to[which(from %in% names(mapping))]
  mapping
}


#' Add faceting variable
#' 
#' @description Convenience function to add default faceting variable
#' 
#' @param facets User input faceting variable as a character or formula.
#' @param variable Default variable to be appended to variables in facets.
#' 
#' @return facets as formula or character string.
#' 
#' @keywords internal
#' @export
add_facet_var <- function(facets, variable = 'variable') {
  if (!is.formula(facets)) {
    c(variable, facets)
  } else {
    stats::update.formula(old = facets, 
                          new = stats::as.formula(stringr::str_c('~. + ', variable)))
  }
}
