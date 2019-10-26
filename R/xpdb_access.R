#' Access model code
#'
#' @description Access model code from an xpdb object.
#'
#' @param xpdb An \code{xpose_data} object from which the model code will be
#'   extracted.
#' @param parsed Should the parsed (\code{parsed = TRUE} (default)) or raw
#'   (\code{parsed = FALSE}) model code be returned.
#' @param .problem The problem to be used (only used with \code{parsed = TRUE}).
#'   Of note, problem 0 is attributed to general output (e.g. NM-TRAN
#'   warnings in NONMEM). When ]code{NULL} (default) returns the entire code.
#' @return A tibble of the parsed model (if \code{parsed = TRUE}) or a character
#'   vector of the raw model (if \code{parsed = FALSE}).
#' @seealso \code{\link{create_xpdb}}, \code{\link{read_nm_model}}
#' @examples
#' parsed_code <- get_code(xpdb_ex_pk)
#' head(parsed_code)
#'
#' raw_code <- get_code(xpdb_ex_pk, parsed = FALSE)
#' head(raw_code)
#'
#' @export
get_code <- function(xpdb, parsed = TRUE, .problem = NULL) {
  check_xpdb(xpdb, check = 'code')
  
  # Get the code
  x <- xpdb$code$code[[which(xpdb$code$parsed == parsed)]]
  
  # Check .problems
  if (!is.null(.problem)) {
    if (parsed) {
      if (!all(.problem %in% x$problem)) {
        stop('$prob no.', stringr::str_c(.problem[!.problem %in% x$problem], collapse = ', '), 
             ' not found in model code.', call. = FALSE)
      }
      x <- x[x$problem %in% .problem, ]
    } else {
      msg('Argument `.problem` ignored with `parsed = FALSE`.', xpdb$options$quiet)
    }
  }
  
  # Output the code
  structure(x, class = c(ifelse(parsed, 'parsed_code', 'raw_code'), class(x)))
}


#' Access model output table data
#'
#' @description Access model output table data from an xpdb object.
#'
#' @param xpdb An \code{xpose_data} object from which the model output file data
#'   will be extracted.
#' @param table Name of the output table to be extracted from the xpdb e.g.
#'   'sdtab001'. Alternative to the `.problem` argument.
#' @param .problem Accesses all tables from the specified problem. Alternative
#'   to the `table` argument.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @return By default returns data from the last estimation problem. If only
#'   simulation problems are present then the data from last simulation will be
#'   returned instead. Object returned as tibble for single tables/problems or a
#'   named list for multiple tables/problems.
#'
#' @seealso  \code{\link{list_data}}, \code{\link{xpose_data}},
#'   \code{\link{read_nm_tables}}
#' @examples
#' # By table name
#' sdtab <- get_data(xpdb_ex_pk, 'sdtab001')
#' sdtab
#'
#' # By problem
#' tables <- get_data(xpdb_ex_pk, .problem = 1)
#' tables
#'
#' # Tip to list available tables in the xpdb
#' print(xpdb_ex_pk)
#'
#' @export
get_data <- function(xpdb, 
                     table    = NULL, 
                     .problem = NULL,
                     quiet) {
  check_xpdb(xpdb, check = 'data')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (is.null(table) && is.null(.problem)) {
    .problem <- default_plot_problem(xpdb)
    msg(c('Returning data from $prob no.', .problem), quiet)
  }
  
  if (!is.null(table) && !is.null(.problem)) {
    stop('Arguments `table` and `.problem` cannot be used together.', call. = FALSE) 
  }
  
  if (!is.null(.problem) && is.na(.problem)) return() # For internal use
  
  x <- xpdb$data
  
  if (!is.null(.problem)) {
    # When selecting tables based on problem level
    if (!all(.problem %in% x$problem)) {
      stop('$prob no.', stringr::str_c(.problem[!.problem %in% x$problem], collapse = ', '), 
           ' not found in model output data.', call. = FALSE)
    }
    x <- x$data[x$problem %in% .problem]
    
    if (length(.problem) > 1) {
      purrr::set_names(x, stringr::str_c('problem_', sort(.problem), sep = ''))
    } else {
      x[[1]]
    }
  } else {
    # When selecting tables based on their name
    full_index <- x %>% 
      dplyr::select(dplyr::one_of('problem', 'index'))
    
    ## TEMP handling
    if (tidyr_new_interface()) {
      full_index <- full_index %>% tidyr::unnest(dplyr::one_of('index'))
    } else {
      full_index <- full_index %>% tidyr::unnest(!!rlang::sym('index'))
    }
    ## END TEMP
    
    if (any(!table %in% full_index$table)) {
      stop(stringr::str_c(table[!table %in% full_index$table], collapse = ', '), 
           ' not found in model output data.', call. = FALSE) 
    }
    x <- full_index[full_index$table %in% table, ] %>% 
      dplyr::group_by_at(.vars = c('problem', 'table'))
    
    ## TEMP handling
    if (tidyr_new_interface()) {
      x <- x %>% tidyr::nest(tmp = -dplyr::one_of('problem', 'table'))
    } else {
      x <- x %>% tidyr::nest(.key = 'tmp')
    }
    ## END TEMP
    
    x <- x %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(cols = purrr::map(.$tmp, ~.$col)) %>% 
      dplyr::group_by_at(.vars = 'table')
    
    ## TEMP handling
    if (tidyr_new_interface()) {
      x <- x %>% tidyr::nest(tmp = -dplyr::one_of('table'))
    } else {
      x <- x %>% tidyr::nest(.key = 'tmp')
    }
    ## END TEMP
    
    x <- x %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(out = purrr::map(.$tmp, function(y) {
        xpdb$data[xpdb$data$problem == y$problem, ]$data[[1]][, y$cols[[1]]]
      }))
    
    if (length(unique(x$table)) > 1) {
      purrr::set_names(x$out, x$table)
    } else {
      x$out[[nrow(x)]]
    }
  }
}


#' Access model output file data
#'
#' @description Access model output file data from an xpdb object.
#'
#' @param xpdb An \code{xpose_data} object from which the model output file data
#'   will be extracted.
#' @param file Full name of the file to be extracted from the xpdb e.g.
#'   'run001.phi'. Alternative to the 'ext' argument.
#' @param ext Extension of the file to be extracted from the xpdb e.g. 'phi'.
#'   Alternative to the 'file' argument.
#' @param .problem The problem to be used, by default returns the last one for
#'   each file.
#' @param .subprob The subproblem to be used, by default returns the last one
#'   for each file.
#' @param .method The estimation method to be used (e.g. 'foce', 'imp', 'saem'),
#'   by default returns the last one for each file.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @return A tibble for single file or a named list for multiple files.
#' @seealso  \code{\link{list_files}}, \code{\link{xpose_data}},
#'   \code{\link{read_nm_files}}
#' @examples
#' # Single file (returns a tibble)
#' ext_file <- get_file(xpdb_ex_pk, file = 'run001.ext')
#' ext_file
#'
#' # Multiple files (returns a list)
#' files <- get_file(xpdb_ex_pk, file = c('run001.ext', 'run001.phi'))
#' files
#'
#' # Tip to list available files in the xpdb
#' print(xpdb_ex_pk)
#'
#' @export
get_file <- function(xpdb, 
                     file     = NULL, 
                     ext      = NULL, 
                     .problem = NULL, 
                     .subprob = NULL, 
                     .method  = NULL, 
                     quiet) {
  check_xpdb(xpdb, check = 'files')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (is.null(file) && is.null(ext)) {
    stop('Argument `file` or `ext` required.', call. = FALSE) 
  }
  
  if (!is.null(file) && !is.null(ext)) {
    stop('Argument `file` and `ext` should not be used simultaneously.', call. = FALSE) 
  }
  
  # Get file name from extension
  if (!is.null(ext)) {
    file <- unique(xpdb$files$name[xpdb$files$extension %in% ext])
    if (length(file) == 0) {
      stop('File extension ', stringr::str_c(ext[!ext %in% xpdb$files$extension], collapse = ', '), ' not found in model output files.', call. = FALSE) 
    }
  }
  
  # Filter by file
  if (any(!file %in% xpdb$files$name)) {
    stop(stringr::str_c(file[!file %in% xpdb$files$name], collapse = ', '), 
         ' not found in model output files.', call. = FALSE) 
  }
  x <- xpdb$files[xpdb$files$name %in% file, ]
  
  # Filter by $problem
  if (is.null(.problem)) {
    .problem <- last_file_problem(xpdb, ext = x$extension)
  } else if (!all(.problem %in% x$problem)) {
    stop('$prob no.', stringr::str_c(.problem[!.problem %in% x$problem], collapse = ', '), 
         ' not found in ', stringr::str_c(unique(x$name), collapse = ', '), ' files.', call. = FALSE)
  }
  x <- x[x$problem %in% .problem, ]
  
  # Filter by sub-problem
  if (is.null(.subprob)) {
    .subprob <- last_file_subprob(xpdb, ext = x$extension, .problem = .problem)
  } else if (!all(.subprob %in% x$subprob)) {
    stop('Subprob no.', stringr::str_c(.subprob[!.subprob %in% x$subprob], collapse = ', '), 
         ' not found in ', stringr::str_c(unique(x$name), collapse = ', '), ' files.', call. = FALSE)
  }
  x <- x[x$subprob %in% .subprob, ]
  
  # Filter by method
  if (is.null(.method)) {
    .method <- last_file_method(xpdb, ext = x$extension, .problem = .problem, .subprob = .subprob)
  } else if (!all(.method %in% x$method)) {
    stop('Method ', stringr::str_c(.method[!.method %in% x$method], collapse = ', '), 
         ' not found in ', stringr::str_c(unique(x$name), collapse = ', ') , ' files.', call. = FALSE)
  }
  x <- x[x$method %in% .method, ]
  
  # Prepare output
  if (nrow(x) > 1) {
    msg(c('Returning data from ', stringr::str_c(unique(x$name), collapse = ', ')), quiet)
    x$data %>% 
      purrr::set_names(stringr::str_c(x$name, '_prob_', x$problem, 
                                      '_subprob_', x$subprob, '_', 
                                      x$method)) %>% 
      return()
  } else {
    msg(c('Returning data from ', x$name, ', $prob no.', x$problem , 
          ', subprob no.', x$subprob, ', method ', x$method), quiet)
    return(x$data[[1]])
  }
}


#' Access model summary data
#'
#' @description Access model summary data from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the summary data will be extracted.
#' @param .problem The .problem to be used, by default returns the last one for each label.
#' @param .subprob The subproblem to be used, by default returns the last one for each label.
#' @param only_last Logical, if \code{TRUE} only the last record for each label is returned in case 
#' of multiple problem and/or subproblem. If \code{FALSE} all values are returned.
#' 
#' @return A tibble of model summary.
#' @seealso \code{\link{xpose_data}}, \code{\link{template_titles}}, \code{\link{summary.xpose_data}}
#' @examples
#' run_summary <- get_summary(xpdb_ex_pk)
#' run_summary
#' 
#' @export
get_summary <- function(xpdb, 
                        .problem  = NULL, 
                        .subprob  = NULL, 
                        only_last = FALSE) {
  check_xpdb(xpdb, check = 'summary')
  x <- xpdb$summary
  
  # Filter by $problem
  if (!is.null(.problem)) {
    if (!all(.problem %in% x$problem)) {
      stop('$prob no.', stringr::str_c(.problem[!.problem %in% x$problem], collapse = ', '), 
           ' not found in model summary.', call. = FALSE)
    }
    x <- x[x$problem %in% .problem, ]
  }
  
  # Filter by sub-problem
  if (!is.null(.subprob)) {
    if (!all(.subprob %in% x$subprob)) {
      stop('Subprob no.', stringr::str_c(.subprob[!.subprob %in% x$subprob], collapse = ', '), 
           ' not found in model summary.', call. = FALSE)
    }
    x <- x[x$subprob %in% .subprob, ]
  }
  
  # Remove duplicates
  if (only_last) x <- x[!duplicated(x$label, fromLast = TRUE), ]
  
  x
}


#' Access model parameters
#'
#' @description Access model parameter estimates from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the model output file data will be extracted.
#' @param .problem The problem to be used, by default returns the last one for each file.
#' @param .subprob The subproblem to be used, by default returns the last one for each file.
#' @param .method The estimation method to be used, by default returns the last one for each file
#' @param digits The number of significant digits to be displayed.
#' @param transform Should diagonal OMEGA and SIGMA elements be transformed to standard deviation and 
#' off diagonal elements be transformed to correlations. 
#' @param show_all Logical, whether the 0 fixed off-diagonal elements should be removed from the output.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @return A tibble for single problem/subprob or a named list for multiple problem|subprob.
#' @seealso \code{\link{prm_table}}
#' @examples
#' # Store the parameter table
#' prm <- get_prm(xpdb_ex_pk, .problem = 1)
#' 
#' # Display parameters to the console
#' prm_table(xpdb_ex_pk, .problem = 1)
#' 
#' @export
get_prm <- function(xpdb, 
                    .problem  = NULL, 
                    .subprob  = NULL, 
                    .method   = NULL,
                    digits    = 4,
                    transform = TRUE,
                    show_all  = FALSE,
                    quiet) {
  
  check_xpdb(xpdb, check = 'files')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  prm_df <- xpdb$files
  
  if (!any(prm_df$extension == 'ext')) {
    stop('File extension `ext` not found in model output files.' , call. = FALSE) 
  }
  
  if (is.null(.problem)) .problem <- last_file_problem(xpdb, ext = 'ext')
  if (is.null(.subprob)) .subprob <- last_file_subprob(xpdb, ext = 'ext', .problem = .problem)
  if (is.null(.method))  .method  <- last_file_method(xpdb, ext = 'ext', .problem = .problem, .subprob = .subprob)
  
  prm_df <- prm_df %>%  
    dplyr::filter(.$extension %in% c('ext', 'cov'), 
                  .$problem %in% .problem,                    
                  .$subprob %in% .subprob, 
                  .$method %in% .method)
  
  
  if (!any(prm_df$extension == 'ext')) {
    stop('No parameter estimates found for $prob no.', 
         stringr::str_c(.problem, collapse = '/'), ', subprob no. ',
         stringr::str_c(.subprob, collapse = '/'), ', method ',
         stringr::str_c(.method, collapse = '/'), '.', call. = FALSE) 
  }
  
  prm_df <- prm_df %>% 
    dplyr::select(-dplyr::one_of('name', 'modified')) %>% 
    tidyr::spread(key = 'extension', value = 'data')
  
  msg(c('Returning parameter estimates from $prob no.', stringr::str_c(unique(prm_df$problem), collapse = ', '), 
        ', subprob no.', stringr::str_c(unique(prm_df$subprob), collapse = ', '), 
        ', method ', stringr::str_c(unique(prm_df$method), collapse = ', ')), quiet)
  
  prm_df <- prm_df %>% 
    dplyr::mutate(prm_names = purrr::map(.x = as.list(.$problem), .f = function(x, code) {
      
      # Collect parameter names from the model code
      code <- code[code$problem == x & nchar(code$code) > 0,]
      list(theta = code$comment[code$subroutine == 'the'],
           omega = code[code$subroutine == 'ome', ] %>%
             dplyr::filter(!(stringr::str_detect(.$code, 'BLOCK\\(\\d+\\)(?!.*SAME)') & .$comment == '')) %>% 
             {purrr::flatten_chr(.[, 'comment'])},
           sigma = code$comment[code$subroutine == 'sig'])
      
    }, code = xpdb$code)) %>% 
    purrr::transpose() %>% 
    purrr::map(.f = function(data) {
      prm_mean <- grab_iter(ext = data$ext, iter = -1000000000)
      prm_se   <- grab_iter(ext = data$ext, iter = -1000000001)
      prm_fix  <- grab_iter(ext = data$ext, iter = -1000000006)
      
      if (all(is.na(prm_fix))) {
        warning('Iteration `-1000000006` not found in the `.ext` file. Assuming no fixed parameters, check the output carefully.', call. = FALSE)
        prm_fix[is.na(prm_fix)] <- 0
      }
      
      if (transform) {
        if (!is.null(data$cov)) {
          # build covariance matrix from `.cov` file
          prm_cov <- as.data.frame(data$cov) %>% 
            tibble::column_to_rownames('NAME') %>% 
            as.matrix()
        } else {
          # build covariance matrix from `.ext` se
          prm_cov <- diag(prm_se)^2
          attr(prm_cov, 'dimnames') <- list(names(prm_se), names(prm_se))
          
          # No warning when .ext SE not available
          if (!all(is.na(prm_se))) {
            warning('Covariance matrix (`.cov`) not available, RSE for covariance parameters will be incorrect.', call. = FALSE)
          }
        }
        # obtain transformation formulas 
        prm_trans_formula <- get_prm_transformation_formulas(names(prm_mean))
        # transform parameters & calculate var, rse for transformation
        prms <- purrr::map_df(prm_trans_formula, ~transform_prm(.x, mu = prm_mean, sigma = prm_cov, method = 'delta')) %>% 
          dplyr::mutate(se = sqrt(.$variance))
      } else {
        prms <- tibble::tibble(mean = purrr::flatten_dbl(prm_mean), 
                               se   = purrr::flatten_dbl(prm_se)) %>% 
          dplyr::mutate(rse = .$se/abs(.$mean))
      }
      
      prms <- prms %>% 
        dplyr::mutate(fixed = as.logical(as.numeric(prm_fix)),
                      name  = names(prm_mean)) %>%
        dplyr::mutate(type = dplyr::case_when(stringr::str_detect(.$name, 'THETA') ~ 'the',
                                              stringr::str_detect(.$name, 'OMEGA') ~ 'ome',
                                              stringr::str_detect(.$name, 'SIGMA') ~ 'sig'),
                      number = stringr::str_replace_all(.$name, '[^\\d,]+', ''),
                      se     = ifelse(.$fixed, NA_real_, as.numeric(.$se)),
                      rse    = ifelse(.$fixed, NA_real_, abs(as.numeric(.$rse)))) %>%
        tidyr::separate(col = 'number', into = c('m', 'n'), sep = ',', fill = 'right') %>% 
        dplyr::mutate(diagonal = dplyr::if_else(.$m == .$n, TRUE, FALSE)) %>% 
        dplyr::rename(!!rlang::sym('value') := !!rlang::sym('mean')) %>% 
        dplyr::mutate(label = '',
                      value = signif(.$value, digits = digits),
                      se    = signif(.$se, digits = digits),
                      rse   = signif(.$rse, digits = digits),
                      n     = as.numeric(.$n),
                      m     = as.numeric(.$m),
                      order = dplyr::case_when(type == 'the' ~ 1,
                                               type == 'ome' ~ 2,
                                               TRUE ~ 3)) %>% 
        dplyr::arrange_at(.vars = 'order') %>% 
        dplyr::select(dplyr::one_of('type', 'name', 'label', 'value', 'se', 'rse', 'fixed', 'diagonal', 'm', 'n'))
      
      # Assign THETA labels
      n_theta     <- sum(prms$type == 'the')
      theta_names <- data$prm_names$theta
      if (n_theta != length(theta_names)) {
        warning('[$prob no.', data$problem, ', subprob no.', data$subprob, ', ', data$method, 
                '] $THETA labels did not match the number of THETAs in the `.ext` file.', call. = FALSE)
      } else {
        prms$label[prms$type == 'the'] <- theta_names
      }
      
      # Assign OMEGA labels
      n_omega     <- sum(prms$type == 'ome' & prms$diagonal, na.rm = TRUE)
      omega_names <- data$prm_names$omega
      if (n_omega != length(omega_names)) {
        warning('[$prob no.', data$problem, ', subprob no.', data$subprob, ', ', data$method, 
                '] $OMEGA labels did not match the number of OMEGAs in the `.ext` file.', call. = FALSE)
      } else {
        prms$label[prms$type == 'ome' & prms$diagonal] <- omega_names
      }
      
      # Assign SIGMA labels
      n_sigma     <- sum(prms$type == 'sig' & prms$diagonal, na.rm = TRUE)
      sigma_names <- data$prm_names$sigma
      if (n_sigma != length(sigma_names)) {
        warning('[$prob no.', data$problem, ', subprob no.', data$subprob, ', ', data$method, 
                '] $SIGMA labels did not match the number of SIGMAs in the `.ext` file.', call. = FALSE)
      } else {
        prms$label[prms$type == 'sig' & prms$diagonal] <- sigma_names
      }
      
      # Filter_all
      if (!show_all) {
        prms <- dplyr::filter(.data = prms, !(prms$type %in% c('ome', 'sig') & 
                                                prms$value == 0 & !prms$diagonal))
      }
      
      # Add metadata to output
      structure(.Data = prms, file = 'ext', problem = data$problem, 
                subprob = data$subprob, method = data$method)
      
    })
  
  # Format output
  if (length(prm_df) == 1) prm_df <- prm_df[[1]]
  structure(prm_df, class = c('xpose_prm', class(prm_df)))
}


#' Grab parameter for a given iteration number
#' @param ext parameter vs. iteration table
#' @param iter iteration number
#'
#' @keywords internal
#' @export
grab_iter <- function(ext, iter) {
  out <- ext %>% 
    dplyr::filter(.$ITERATION == iter) %>% 
    dplyr::select(-dplyr::one_of('ITERATION', 'OBJ'))
  
  if (nrow(out) == 0) out[1,] <- NA_real_
  
  purrr::flatten(out)
}


#' Generate default transformation formulas for parameters
#'
#' @param prm_names Vector of parameter names as found in .ext 
#'
#' @return List of formulas decribing the transformation
#' @keywords internal
#' @export
get_prm_transformation_formulas <- function(prm_names) {
  prm_names %>% 
    purrr::set_names() %>% 
    purrr::map_if(~stringr::str_detect(.x, "^THETA"), 
                  ~substitute(~par, list(par = rlang::sym(.x)))) %>% 
    purrr::map_if(~purrr::is_character(.x) && stringr::str_detect(.x, '^(OMEGA|SIGMA)\\((\\d+),\\2\\)$'), 
                  ~substitute(~sqrt(par), list(par = rlang::sym(.x)))) %>% 
    purrr::map_if(~purrr::is_character(.x) && stringr::str_detect(.x, '^(OMEGA|SIGMA)\\(\\d+,\\d+\\)$'), 
                  ~substitute(~cov/(sqrt(var1)*sqrt(var2)), 
                              list(cov = rlang::sym(.x),
                                   var1 = stringr::str_replace(.x, '\\((\\d+),(\\d+)\\)', 
                                                               '(\\1,\\1)') %>% rlang::sym(),
                                   var2 = stringr::str_replace(.x, '\\((\\d+),(\\d+)\\)', 
                                                               '(\\2,\\2)') %>% rlang::sym())))
  
}


#' Access special model data
#'
#' @description Access special model data from an xpdb object.
#' 
#' @param xpdb An \code{xpose_data} object from which the special data will be extracted.
#' @param .problem The problem to be used, by default returns the last one.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @return A list.
#' @seealso  \code{\link{list_special}}, \code{\link{xpose_data}}
#' @examples
#' special <- get_summary(xpdb_ex_pk)
#' special
#' 
#' @export
get_special <- function(xpdb, .problem = NULL, quiet) {
  check_xpdb(xpdb, check = 'special')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  x <- xpdb$special
  if (is.null(.problem)) .problem <- max(x$problem)
  
  if (!all(.problem %in% x$problem)) {
    stop('$prob no.', stringr::str_c(.problem[!.problem %in% x$problem], collapse = ', '), 
         ' not found in special data.', call. = FALSE)
  }
  x <- x[x$problem %in% .problem, ]
  
  if (length(.problem) > 1) {
    purrr::set_names(x$data, stringr::str_c('problem_', x$problem, '_', x$method, '_', x$type))
  } else {
    msg(c('Returning ', x$method, ' ' , x$type, ' data from $prob no.', x$problem), quiet)
    x$data[[1]]
  }
}

