#' Set variable type, label or units
#'
#' @description Function designed to change the type, label or unit associated with variables.
#' 
#' @param xpdb An \code{xpose_data} object.
#' @param problem The problem number to which the edits will be applied.
#' @param ... Specifications of the edits to be made to the xpdb index. Edits are made as 
#' type and variable pairs e.g. idv = 'TAD' will assign TAD to the type idv (independent variable).
#' @param auto_factor With \code{set_var_types} only. If \code{TRUE} new columns assigned to the type 'catcov' will be converted to
#' factor.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @section Recognized variable types:
#' \itemize{
#'   \item amt: Dose amount
#'   \item catcov: Categorical covariate
#'   \item contcov: Continuous covariate
#'   \item dv: Dependent variable
#'   \item dvid: DV identifier
#'   \item eta: Eta
#'   \item evid: Event identifier
#'   \item id: Subject identifier
#'   \item idv: Independent variable
#'   \item ipred: Individual model predictions
#'   \item mdv: Missing dependent variable
#'   \item na: Not attributed
#'   \item occ: Occasion flag
#'   \item param: Model parameter
#'   \item pred: Typical model predictions
#'   \item res: Residuals
#'  }
#'  
#' @return An xpose_data object
#' @seealso \code{\link{list_vars}}
#' @examples
#' # Change variable type
#' xpdb_2 <- set_var_types(xpdb_ex_pk, problem = 1, idv = 'TAD')
#' 
#' # Change labels
#' xpdb_2 <- set_var_labels(xpdb_2, problem = 1, ALAG1 = 'Lag time', CL = 'Clearance', V = 'Volume')
#' 
#' # Change units
#' xpdb_2 <- set_var_units(xpdb_2, problem = 1, ALAG1 = 'h', CL = 'L/h', V = 'L')
#' 
#' @name set_vars
#' @export
set_var_types <- function(xpdb, problem = NULL, ..., auto_factor = TRUE, quiet) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  dat <- xpdb$data
  
  if (!is.null(problem) && !all(problem %in% dat$problem)) {
    stop('Problem no.', stringr::str_c(problem[!problem %in% dat$problem], collapse = ', '), 
         ' not found in model output data.', call. = FALSE)
  }
  if (is.null(problem)) problem <- unique(dat$problem)
  
  args <- c(...)
  if (is.null(args)) return(xpdb)
  
  args <- args %>%
    dplyr::data_frame(col = ., type = names(.)) %>% 
    dplyr::mutate(type = stringr::str_replace(.$type, '\\d$', ''))
  
  xpdb$data <- dat %>% 
    dplyr::mutate(grouping = .$problem) %>% 
    dplyr::group_by_(.dots = 'grouping') %>% 
    tidyr::nest(.key = 'tmp') %>% 
    dplyr::mutate(out = purrr::map_if(.$tmp, .$grouping %in% problem, function(x, args, quiet) {
      # Get the index
      index <- x$index[[1]]
      
      # Check for missmatches
      if (any(!args$col %in% index$col)) {
        msg(c('In $prob no.', x$problem, ' columns: ',
              stringr::str_c(args$col[!args$col %in% index$col], collapse = ', '),
              ' not present in the data.'), quiet)
        args <- dplyr::filter(.data = args, args$col %in% index$col)
      }
      
      # Remove previous index when only one variable can be used at the time
      single_type <- c('amt', 'dv', 'dvid', 'evid', 'id', 'idv', 'ipred', 'mdv', 'pred')
      single_type <- single_type[single_type %in% args$type]
      if (length(single_type) > 0) index$type[index$type %in% single_type] <- 'na'
      
      # Replace the matching values
      for (repl in 1:nrow(args)) {
        index$type[index$col == args$col[repl]] <- args$type[repl]
      }
       x$index[[1]] <- index
      
      # Change categorical covariates to factor
      if (any(args$type == 'catcov') && auto_factor) {
        col_to_factor <- colnames(x$data[[1]]) %in% args$col[args$type == 'catcov']
        x$data[[1]] <- x$data[[1]] %>%   
          dplyr::mutate_if(col_to_factor, as.factor)
      }
      
      # Output new index
      x
    }, args = args, quiet = quiet)) %>% 
    tidyr::unnest_(unnest_cols = 'out') %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'index', 'data', 'modified'))
  
  xpdb
}


set_var_generic <- function(xpdb, problem = NULL, what = NULL, ..., quiet) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  dat <- xpdb$data
  
  if (!is.null(problem) && !all(problem %in% dat$problem)) {
    stop('Problem no.', stringr::str_c(problem[!problem %in% dat$problem], collapse = ', '), 
         ' not found in model output data.', call. = FALSE)
  }
  if (is.null(problem)) problem <- unique(dat$problem)
  
  args <- c(...)
  if (is.null(args)) return(xpdb)
  
  args <- dplyr::data_frame(col = names(args), variable = args)
  
  xpdb$data <- dat %>% 
    dplyr::mutate(grouping = .$problem) %>% 
    dplyr::group_by_(.dots = 'grouping') %>% 
    tidyr::nest(.key = 'tmp') %>% 
    dplyr::mutate(out = purrr::map_if(.$tmp, .$grouping %in% problem, function(x, args, quiet) {
      # Get the index
      index <- x$index[[1]]
      
      # Check for missmatches
      if (any(!args$col %in% index$col)) {
        msg(c('In $prob no.', x$problem, ' columns: ',
              stringr::str_c(args$col[!args$col %in% index$col], collapse = ', '),
              ' not present in the data.'), quiet)
        args <- dplyr::filter(.data = args, args$col %in% index$col)
      }
      
      # Replace the matching values
      index[match(args$col, index$col), what] <- args$variable
      x$index[[1]] <- index
      
      # Output new index
      x
    }, args = args, quiet = quiet)) %>% 
    tidyr::unnest_(unnest_cols = 'out') %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'index', 'data', 'modified'))
  
  xpdb
}

#' @rdname set_vars
#' @export
set_var_labels <- function(xpdb, problem = NULL, ..., quiet) {
  set_var_generic(xpdb = xpdb, problem = problem, 
                  quiet = quiet, what = 'label', ...)
}

#' @rdname set_vars
#' @export
set_var_units <- function(xpdb, problem = NULL, ..., quiet) {
  set_var_generic(xpdb = xpdb, problem = problem, 
                  quiet = quiet, what = 'units', ...)
}
