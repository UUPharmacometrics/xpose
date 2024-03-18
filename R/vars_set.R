#' Set variable type, label or units
#'
#' @description Function designed to change the type, label or unit associated with variables.
#' 
#' @param xpdb An \code{xpose_data} object.
#' @param .problem The problem number to which the edits will be applied.
#' @param ... Specifications of the edits to be made to the xpdb index. Edits are made as 
#' type and variable pairs e.g. idv = 'TAD' will assign TAD to the type idv (independent variable).
#' @param info A data frame of variable names, labels, and units.
#' @param auto_factor With \code{set_var_types} only. If \code{TRUE} new columns assigned to the type 'catcov' will be converted to
#' factor.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @section Recognized variable types:
#' \itemize{
#'   \item a: Compartments' amount
#'   \item amt: Dose amount
#'   \item catcov: Categorical covariate
#'   \item contcov: Continuous covariate
#'   \item dv: Dependent variable
#'   \item dvid: DV identifier
#'   \item eta: Eta
#'   \item evid: Event identifier
#'   \item id: Subject identifier
#'   \item idv: Independent variable
#'   \item tad: Time after dose
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
#' xpdb_2 <- set_var_types(xpdb_ex_pk, .problem = 1, catcov = 'DOSE')
#' 
#' # Change labels
#' xpdb_2 <- set_var_labels(xpdb_2, .problem = 1, ALAG1 = 'Lag time', CL = 'Clearance', V = 'Volume')
#' 
#' # Change units
#' xpdb_2 <- set_var_units(xpdb_2, .problem = 1, ALAG1 = 'h', CL = 'L/h', V = 'L')
#' 
#' labels_units <- data.frame(
#'   col = c('ALAG1', 'CL', 'V'),
#'   label = c('Lag time', 'Clearance', 'Volume'),
#'   units = c('h', 'L/h', 'L')
#' )
#' 
#' xpdb_2 <- set_var_labels_units(xpdb_ex_pk, .problem = 1, info = labels_units)
#' 
#' @name set_vars
#' 
#' @return A xpose object
#' @export
#' 
#' 
set_var_types <- function(xpdb, .problem = NULL, ..., auto_factor = TRUE, quiet) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  dat <- xpdb$data
  
  if (!is.null(.problem) && !all(.problem %in% dat$problem)) {
    stop('Problem no.', stringr::str_c(.problem[!.problem %in% dat$problem], collapse = ', '), 
         ' not found in model output data.', call. = FALSE)
  }
  if (is.null(.problem)) .problem <- unique(dat$problem)
  
  args <- c(...)
  if (is.null(args)) return(xpdb)
  
  args <- args %>%
    dplyr::tibble(col = ., type = names(.)) %>% 
    dplyr::mutate(type = stringr::str_replace(.$type, '\\d$', ''))
  
  dat <- dat %>% 
    dplyr::mutate(grouping = .$problem) %>% 
    dplyr::group_by_at(.vars = 'grouping')
  
  ## TEMP handling
  if (tidyr_new_interface()) {
    dat <- dat %>% tidyr::nest(tmp = -dplyr::one_of('grouping'))
  } else {
    dat <- dat %>% tidyr::nest(.key = 'tmp')
  }
  ## END TEMP
  
  xpdb$data <- dat %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(out = purrr::map_if(.$tmp, .$grouping %in% .problem, function(x, args, quiet) {
      # Get the index
      index <- x$index[[1]]
      
      # Check for missmatches
      if (any(!args$col %in% index$col)) {
        warning(c('In $prob no.', x$problem, ' columns: ',
                  stringr::str_c(args$col[!args$col %in% index$col], collapse = ', '),
                  ' not present in the data.'), call. = FALSE)
        args <- dplyr::filter(.data = args, args$col %in% index$col)
      }
      
      # Remove previous index when only one variable can be used at the time
      single_type <- c('amt', 'dv', 'dvid', 'evid', 'id', 'idv', 'tad', 'ipred', 'mdv', 'pred')
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
    tidyr::unnest(dplyr::one_of('out')) %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'index', 'data', 'modified'))
  
  as.xpdb(xpdb)
}


set_var_generic <- function(xpdb, .problem = NULL, what = NULL, ..., quiet) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  dat <- xpdb$data
  
  if (!is.null(.problem) && !all(.problem %in% dat$problem)) {
    stop('Problem no.', stringr::str_c(.problem[!.problem %in% dat$problem], collapse = ', '), 
         ' not found in model output data.', call. = FALSE)
  }
  if (is.null(.problem)) .problem <- unique(dat$problem)
  
  args <- c(...)
  if (is.null(args)) return(xpdb)
  
  args <- dplyr::tibble(col = names(args), variable = args)
  
  dat <- dat %>% 
    dplyr::mutate(grouping = .$problem) %>% 
    dplyr::group_by_at(.vars = 'grouping')
  
  ## TEMP handling
  if (tidyr_new_interface()) {
    dat <- dat %>% tidyr::nest(tmp = -dplyr::one_of('grouping'))
  } else {
    dat <- dat %>% tidyr::nest(.key = 'tmp')
  }
  ## END TEMP
  
  xpdb$data <- dat %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(out = purrr::map_if(.$tmp, .$grouping %in% .problem, function(x, args, quiet) {
      # Get the index
      index <- x$index[[1]]
      
      # Check for missmatches
      if (any(!args$col %in% index$col)) {
        warning(c('In $prob no.', x$problem, ' columns: ',
                  stringr::str_c(args$col[!args$col %in% index$col], collapse = ', '),
                  ' not present in the data.'), call. = FALSE)
        args <- dplyr::filter(.data = args, args$col %in% index$col)
      }
      
      # Replace the matching values
      index[match(args$col, index$col), what] <- args$variable
      x$index[[1]] <- index
      
      # Output new index
      x
    }, args = args, quiet = quiet)) %>% 
    tidyr::unnest(dplyr::one_of('out')) %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'index', 'data', 'modified'))
  
  as.xpdb(xpdb)
}

#' @rdname set_vars
#' @export
set_var_labels <- function(xpdb, .problem = NULL, ..., quiet) {
  set_var_generic(xpdb = xpdb, .problem = .problem, 
                  quiet = quiet, what = 'label', ...)
}

#' @rdname set_vars
#' @export
set_var_units <- function(xpdb, .problem = NULL, ..., quiet) {
  set_var_generic(xpdb = xpdb, .problem = .problem, 
                  quiet = quiet, what = 'units', ...)
}

#' @rdname set_vars
#' @export
set_var_labels_units <- function(
    xpdb,
    .problem = NULL,
    info = NULL
){
  
  check_xpdb(xpdb, check = 'data')
  
  if ( !is.data.frame(info) ){
    stop('Invalid info argument')
  }
  
  if ( !all(names(info) == c('col', 'label', 'units')) ){
    stop('Invalid data frame structure in info')
  }
  
  info <- info %>% 
    select( col, label, units )
  
  if ( nrow(info) > nrow(unique(info)) ){
    stop('Duplicate variable entries in info')
  }
  
  if ( is.null(.problem) ){
    .problem <- 1:length(xpdb$data$index)
  }
  
  for (.prob in .problem){
    # Filter info to distinct variables present in data
    tmp <- info %>%
      dplyr::distinct( col, .keep_all = TRUE ) %>%
      dplyr::filter( col %in% names(get_data(xpdb, .problem = .prob)) )
    
    # Collapse labels and units information into strings
    var_labels <- c()
    var_units <- c()
    for ( i in seq_len(nrow(tmp)) ){
      if ( is.na(tmp$label[i]) ){
        var_labels <- c( var_labels, paste0(tmp$col[i], '  = NA') )
      } else {
        var_labels <- c( var_labels, paste0(tmp$col[i], '  = \'', tmp$label[i], '\'') )
      }
      if ( is.na(tmp$units[i]) ){
        var_units <- c( var_units, paste0(tmp$col[i], '  = NA') )
      } else {
        var_units <- c( var_units, paste0(tmp$col[i], '  = \'', tmp$units[i], '\'') )
      }
    }
    
    var_labels <- paste(var_labels, collapse = ', ')
    var_units <- paste(var_units, collapse = ', ')
    
    # Apply labels and units
    if ( var_labels != '' ){
      eval(
        parse(
          text = sprintf(
            'xpdb <- set_var_labels(xpdb, .problem = %s, %s)',
            .prob,
            var_labels
          )
        )
      )
    }
    if ( var_units != '' ){
      eval(
        parse(
          text = sprintf(
            'xpdb <- set_var_units(xpdb, .problem = %s, %s)',
            .prob,
            var_units
          )
        )
      )
    }
  }
  
  xpdb <- unclass(xpdb)
  
  xpdb$label_units <- info
  
  as.xpdb(xpdb)
}

#' @title Format numerical variables in data tables
#'
#' @description This function applies SAS-like formats to the numerical variables
#' according to a user-defined table of format definitions. Formatting is
#' performed by transforming the variables into factors. Discrete numerical 
#' variables can become discrete character variables more suited for reporting 
#' purposes. Additionally, formats can be used to discretize continuous 
#' variables by defining ranges of values associated with specific formats.
#'  
#' @param .data A data.frame or tibble of data
#' @param formats A data.frame with an expected structure defined in the Details
#' section
#'
#' @return An updated data object with the same dimension as the \code{.data}
#' argument.
#'
#' @details
#' Formats typically are intended to be applied to categorical variables (eg, sex)
#' to replace numerical values by character labels (eg, 0=Males, 1=Females).
#' However, formats can also be applied to continuous variables (eg, CrCL) to
#' coerce them into categorical variables by defining intervals (eg, >90=Normal
#' renal function).
#'
#' Format data.frames have the following expected structure:
#' \describe{
#'  \item{VARIABLE}{The (case sensitive) name of the variable to apply this
#'  format to.}
#'  \item{START}{The start of the interval. If missing, -Inf is assumed.}
#'  \item{END}{The end of the interval (start and end can be identical when
#'  dealing with format for categorical variables). If missing, +Inf is
#'  assumed.}
#'  \item{EXCLS}{Whether the start value of the interval should be excluded.
#'  Can be set to 0/1 or FALSE/TRUE.}
#'  \item{EXCLE}{Whether the end value of the interval should be excluded.
#'  Can be set to 0/1 or FALSE/TRUE.}
#'  \item{LABEL}{The format label to be applied for value of VARIABLE within
#'  START and END}
#'  \item{ORDER}{Ordering index of the interval}
#'  }
#'  
#'  The complete definition of a variable format can and will typically span
#'  multiple rows of the format data.frames (eg, for sex, 2 rows will be
#'  required: one for the 0 value, and another one for the 1 value). Format
#'  data.frames can contain format for variables which do not exist in \code{.data}.
#'  
#'  Data formatting will not be performed if the format definition for a variable
#'  is invalid. A format definition will be deemed invalid if: \itemize{
#'  \item{formatting information is not provided for any variables included in
#'  the data,}
#'  \item{formatting information is not provided for all values included in the
#'  data,}
#'  \item{formatting information is provided for non-numeric variables,}
#'  \item{EXCLS, EXCLE, LABEL, and/or ORDER are missing,}
#'  \item{ranges defined by START and END overlaps, or}
#'  \item{LABEL and ORDER are not consistent.}
#'  }
#'  
#'  Note that while LABEL and ORDER must be consistent, several values (or
#'  ranges of values) of a variable can be set to the same LABEL and ORDER.
#'  
#' @examples
#' fmts <- data.frame(
#'   variable = c("SEX", "SEX", "CLCR", "CLCR", "CLCR", "CLCR", "CLCR"),
#'   start = c(1L, 2L, 90L, 60L, 30L, 15L, NA),
#'   end = c(1L, 2L, NA, 90L, 60L, 30L, 15L), 
#'   excls = c(0L, 0L, 1L, 1L, 1L, 1L,  0L),
#'   excle = c(0L, 0L, 0L, 0L, 0L, 0L, 0L), 
#'   label = c("Male", "Female", "Normal", "Mild", "Moderate", "Severe", "End of disease"),
#'   order = c(1L, 2L, 1L, 2L, 3L, 4L, 5L)
#' )
#' fmts
#' xp2 <- apply_formats(xpdb, .problem = 1, fmts) 
#' 
#' @rdname apply_formats
#'
#' @export
#'

apply_formats <- function( 
    xpdb, 
    .problem,
    formats,
    quiet 
){
  
  are_range_overlapping <- function(ranges){
    
    if ( nrow(ranges) == 1 ){
      return(FALSE)
    }
    ranges <- as.data.frame(ranges)
    
    ranges <- ranges[order(ranges[,1]),]
    
    if ( any(ranges[2,1:2]-ranges[1,1:2] < 0) ){
      stop('Range end(s) larger than range start(s)')
    }
    
    # Check if edges overlap
    edges <- c(t(as.matrix(ranges[,1:2])))
    
    # Check if same edge is included in 2 consecutive interval
    excls <- c(t(as.matrix(ranges[,3:4])))
    nr <- nrow(ranges)
    same_edge <- diff(edges)[2*(1:(nr-1))]==0 & diff(excls)[2*(1:(nr-1))]==0 &
      ranges[-1,3]==0
    
    is.unsorted(edges) | any(same_edge)
    
  }
  
  df_collapse <- function(x){
    do.call(paste, c(unname(x), sep = '@'))
  }
  
  if ( missing(quiet) )
    quiet <- !interactive()
  
  # Check xpdb
  check_xpdb(xpdb, check = 'data')
  
  if ( missing(.problem) ){
    .problem <- default_plot_problem(xpdb)
  }
  
  data <- get_data( xpdb, .problem = .problem ) %>% 
    dplyr::ungroup()
  
  # Check formats
  if ( missing(formats) ){
    stop('Missing `formats` argument')
  }
  # formats must be a data.frame or a tibble
  if ( !(is.data.frame(formats) | dplyr::is.tbl(formats)) ){
    stop( 'Invalid `formats` argument' )
  }
  names(formats) <- toupper( names(formats) )
  
  formats_col <- c('VARIABLE', 'START', 'END', 'EXCLS', 'EXCLE', 'LABEL', 'ORDER')
  if ( !all( formats_col %in% names(formats) ) ){
    stop( 'Missing columns in `formats` argument' )
  }
  
  formats <- formats %>%
    dplyr::ungroup() %>%
    dplyr::select( VARIABLE, START, END, EXCLS, EXCLE, LABEL, ORDER )
  
  if ( !is.character(formats$VARIABLE) | !is.character(formats$LABEL) ) {
    stop('VARIABLE and LABEL columns in `formats` argument must be character')
  }
  
  if ( !is.numeric(formats$START) | !is.numeric(formats$END) | !is.numeric(formats$ORDER) ) {
    stop('START, END, and ORDER columns in `formats` argument must be numeric')
  }
  
  if (
    !(is.numeric(formats$EXCLS) | is.logical(formats$EXCLS)) |
    !(is.numeric(formats$EXCLE) | is.logical(formats$EXCLE))
  ){
    stop('EXCLS and EXCLE columns in `formats` argument must be integer or logical')
  }
  
  if ( nrow(formats) == 0 ){
    if ( !quiet )
      warning( 'No available format. Formatting aborted.' )
    return( xpdb )
  }
  
  # Pre-process formats
  formats <- formats %>%
    # Ensure labels are characters
    dplyr::mutate( LABEL = as.character(LABEL) ) %>%
    # Filter out from formats variables which are not in data
    dplyr::filter( VARIABLE %in% names(data) ) %>%
    # Replace missing min with -inf and missing max with +inf
    dplyr::mutate(
      START = ifelse( is.na(START), -Inf, START ),
      END = ifelse( is.na(END), +Inf, END ),
    ) %>%
    # Coerce exclusion columns to integer
    dplyr::mutate(
      EXCLS = as.integer(as.logical(EXCLS)),
      EXCLE = as.integer(as.logical(EXCLE))
    ) %>%
    # Filter out duplicated format rows
    dplyr::distinct()
  
  if ( nrow(formats) == 0 ){
    if ( !quiet )
      warning( 'Variables in data have no available format' )
    return( xpdb )
  }
  
  # Trap formats with empty rows
  empty_vars <- formats %>%
    dplyr::filter(
      START == -Inf & END == Inf & is.na(EXCLS) & is.na(EXCLE) & is.na(LABEL) & is.na(ORDER)
    ) %>%
    dplyr::distinct( VARIABLE) %>%
    dplyr::pull( VARIABLE )
  
  if ( length(empty_vars) > 0 ){
    formats <- formats %>%
      dplyr::filter( !VARIABLE %in% empty_vars )
    if ( !quiet ){
      warning(
        sprintf(
          paste(
            'Format not applied for the following variables',
            'because of empty rows in format definition:\n  %s\n'),
          paste( empty_vars, collapse = ', ')
        )
      )
    }
  }
  
  # Trap formats with missing (NA) information
  if ( any(is.na(formats)) ){
    na_vars <- formats %>%
      dplyr::mutate( CHK = dplyr::if_any(.fns = is.na) ) %>%
      dplyr::filter( CHK == TRUE ) %>%
      dplyr::pull( VARIABLE )
    formats <- formats %>%
      dplyr::filter( !VARIABLE %in% na_vars )
    if ( !quiet ){
      warning(
        sprintf(
          paste(
            'Format not applied for the following variable(s)',
            'because of missing values:\n  %s\n'),
          paste( na_vars, collapse = ', ')
        )
      )
    }
  }
  
  if ( nrow(formats) == 0 ){
    if ( !quiet )
      warning( 'Variables in data have no available format' )
    return( xpdb )
  }
  
  # Check for consistency of label and order numbers within variables
  if (
    formats %>% dplyr::distinct( VARIABLE, LABEL) %>% nrow() !=
    formats %>% dplyr::distinct( VARIABLE, ORDER) %>% nrow()
  ){
    if ( !quiet )
      warning(
        paste(
          'Format not applied because the numbers of unique',
          'format labels and orders\n  was not consistent for all variables'
        )
      )
    return( xpdb )
  }
  
  tmp <- suppressWarnings(
    formats %>% dplyr::distinct( VARIABLE, LABEL) %>% rownames() ==
      formats %>% dplyr::distinct( VARIABLE, ORDER) %>% rownames()
  )
  if ( !all(tmp) ){
    vars <- formats[!tmp, 'VARIABLE']
    if ( !quiet )
      warning(
        sprintf(
          paste(
            'Format not applied because the indexes of unique format labels and',
            'orders\n  were not consistent for the following variables:\n  %s'
          ),
          paste(vars, collapse = ', ')
        )
      )
    return( xpdb )
  }
  
  # Store formats for later usage
  oformats <- formats
  
  # Check that formats do not overlap
  error_fvars <- c()
  fvars <- unique(formats$VARIABLE)
  
  for (var in fvars){
    tmp <- try(
      formats %>%
        dplyr::filter( VARIABLE == var) %>%
        dplyr::select( START, END, EXCLS, EXCLE ) %>%
        are_range_overlapping(),
      silent = TRUE
    )
    if ( class(tmp)=='try-error' || tmp ){
      error_fvars <- c(error_fvars, var)
    }
  }
  
  # Store formats for later usage
  oformats <- formats
  
  if ( length(fvars) > 0 && length(error_fvars) > 0){
    fvars <- fvars[ !fvars%in%error_fvars ]
    formats <- formats %>%
      dplyr::filter( VARIABLE %in% fvars )
    if ( !quiet )
      warning(
        sprintf(
          paste(
            'Format not applied for the following variables because of invalid or',
            '\n  overlapping ranges:\n  %s\n'
          ),
          paste(error_fvars, collapse = ', ')
        )
      )
  }
  
  # Prevent formatting of character variables
  error_fvars <- c()
  for ( var in fvars ){
    if ( mode(data[[var]]) != 'numeric' ){  #note: mode also detects numeric factor
      error_fvars <- c(error_fvars, var)
    }
  }
  
  if ( length(error_fvars) > 0 ){
    formats <- formats %>%
      dplyr::filter( !VARIABLE %in% error_fvars )
    if ( !quiet )
      warning(
        sprintf(
          'Non-numeric variable(s) cannot be formatted:\n %s\n',
          paste(error_fvars, collapse = ', ')
        )
      )
  }
  
  fvars <- formats %>%
    dplyr::distinct( VARIABLE ) %>%
    dplyr::pull( VARIABLE )
  
  if ( nrow(formats) == 0 ){
    if ( !quiet )
      warning( 'Variables in data have no available format' )
    return( xpdb )
  }
  
  # Apply formats
  
  # Copy original variables
  odata <- data %>%
    dplyr::select( dplyr::any_of(fvars) )
  
  # Determine which formats are defined with ranges
  formats <- formats %>%
    dplyr::mutate( ISRANGE = ifelse( abs(END - START) > 0, 1, 0) )
  
  tmp <- formats %>%
    dplyr::group_by( VARIABLE ) %>%
    dplyr::summarize(
      ISRANGE = max(ISRANGE),
      .groups = 'keep'
    )
  fvars_byrange <- tmp %>%
    dplyr::filter( ISRANGE == 1 ) %>%
    dplyr::pull( VARIABLE )
  fvars_byval <- tmp %>%
    dplyr::filter( ISRANGE == 0 ) %>%
    dplyr::pull( VARIABLE )
  
  # Coerce EXCLS and EXCLE to 0 for fvars_byval
  if (
    formats %>%
    dplyr::filter( VARIABLE %in% fvars_byval ) %>%
    dplyr::filter( EXCLS == 1L | EXCLE == 1L) %>%
    nrow() > 0
  ){
    if ( !quiet )
      message(
        sprintf(
          paste(
            'Inclusion of start and end range values was assumed',
            'for the following variables:\n  %s\n'),
          paste(fvars_byval, collapse = ', ')
        )
      )
  }
  formats <- formats %>%
    dplyr::mutate(
      EXCLS = ifelse( VARIABLE %in% fvars_byval, 0L, EXCLS),
      EXCLE = ifelse( VARIABLE %in% fvars_byval, 0L, EXCLE)
    )
  
  # Format variables in data
  done <- c()
  messages <- c()
  nformats <- formats[c(),]
  
  for ( var in fvars ){
    
    # Reset variable to NA
    data[[var]] <- NA
    
    # Get format for var
    format <- formats %>%
      dplyr::filter( VARIABLE == var ) %>%
      dplyr::distinct()
    
    # Subset format to values found in data, only for format defined by value
    tmp <- format$START %in% unique(data[[var]])
    if ( var %in% fvars_byval && sum(tmp) > 0 ){
      format <- format %>%
        dplyr::filter( START %in% unique(data[[var]]) )
    }
    
    # Re-order format
    format <- format %>%
      dplyr::arrange( ORDER )
    format$ORDER <- match( format$LABEL, unique(format$LABEL) )
    
    # Convert to numeric data
    if ( mode(odata[[var]]) == 'numeric' & inherits(odata[[var]], 'factor') ){
      vdata <- as.numeric( levels(odata[[var]]) )[ odata[[var]] ]
    } else {
      vdata <- odata[[var]]
    }
    
    # Detect if variables include NA's, substitute value and add format if this
    # is the case
    hasNA <- FALSE
    if ( any(is.na(vdata)) ){
      hasNA <- TRUE
      
      # Find replacement info
      missingVal <- -99
      n <- 3
      while ( missingVal %in% vdata ){
        missingVal <- -sum(sapply(1:n, function(n) 9*10^(n-1)))
        n <- n+1
      }
      replacement <- 'Missing'
      while ( any(format$LABEL == replacement) ){
        replacement <- sprintf('_%s_', replacement)
      }
      
      # Replace in vdata
      vdata[ is.na(vdata) ] <- missingVal
      
      # Replace NA in odata
      if ( is.factor(odata[[var]]) ){
        odata[[var]] <- factor(
          odata[[var]],
          levels = c(levels(odata[[var]]), NA),
          labels = c(levels(odata[[var]]), missingVal),
          exclude = NULL
        )
      } else {
        odata[is.na(odata[[var]]), var] <- missingVal
      }
      
      # Add format
      format <- format %>%
        dplyr::bind_rows(
          data.frame(
            VARIABLE = var,
            START = missingVal,
            END = missingVal,
            EXCLS = 0L,
            EXCLE = 0L,
            LABEL = replacement,
            ORDER = max(format$ORDER) + 1
          )
        )
      
      messages <- c(
        messages,
        sprintf('Missing values were set to %s for variable %s\n', missingVal, var)
      )
    }
    
    # Update nformat
    nformats <- dplyr::bind_rows( nformats, format )
    
    # Replace NA by order in var
    # Add 1e-12 left and right of interval (proportionally) when >= or <= is  
    # used to deal with numerical representation issues
    fs <- c()
    
    for ( f in 1:nrow(format) ){
      
      matches <- eval(
        parse(
          text = paste(
            'vdata >',
            ifelse( format$EXCLS[f] == 0, '= ', ' ' ),
            ifelse(
              format$EXCLS[f] == 0,
              ifelse(
                format$START[f] >= 0,
                format$START[f]*(1-1e-12),
                format$START[f]*(1+1e-12)
              ),
              format$START[f]
            ),
            ' & ','vdata <',
            ifelse( format$EXCLE[f] == 0, '= ', ' ' ),
            ifelse(
              format$EXCLE[f] == 0,
              ifelse(
                format$END[f] >= 0,
                format$END[f]*(1+1e-12),
                format$END[f]*(1-1e-12)
              ),
              format$END[f]
            ),
            sep = ''
          )
        )
      )
      matches <- !is.na(matches) & matches
      if ( length(matches) > 0){
        data[matches, var] <- format$ORDER[f]
      }
      if ( any(matches) ){
        fs <- c(fs, f)
      }
    }
    
    # Check if remaining NA's (ie, label was not provided for all variable
    # level in NONMEM table): if yes, revert to original var data; otherwise,
    # apply labels and set var to formatted factors
    
    if ( any(is.na(data[[var]])) ){
      misvals <- unique( odata[is.na(data[[var]]),var] )
      nmisvals <- length(misvals)
      misvals <- misvals[ 1:min(c(10, nmisvals)) ]
      messages <- c(
        messages,
        sprintf(
          paste(
            'Format not applied for %s because',
            'labels were not provided for the\n  following value(s)%s: %s\n'),
          var,
          ifelse(nmisvals>10,' (only 10 shown)',''),
          paste(misvals, collapse=', ')
        )
      )
      data[[var]] <- odata[[var]]
    } else {
      data[[var]] <- factor(
        data[[var]],
        labels = unique(format$LABEL[fs])
      )
      class(data[[var]]) <- c('factor','formatted')
      done <- c(done, var)
    }
  }
  
  # List formatted variables
  if ( length(done) > 0 ){
    messages <- c(
      messages,
      sprintf(
        paste(
          'The following variables have been formatted and',
          'coerced to factors:\n  %s\n'),
        paste(done, collapse=', ')
      )
    )
  }
  
  if ( !quiet )
    cat(
      sprintf(
        '\n%s',
        paste(messages, collapse='\n')
      )
    )
  
  # Store information
  
  # Bring back oformats
  # Add original order variable in formats and nformats
  formats <- oformats %>%
    dplyr::mutate(
      O_ORDER = ORDER,
      ORDER = NA
    )
  nformats <- nformats %>%
    dplyr::select( -ISRANGE ) %>%
    dplyr::mutate( O_ORDER = NA )
  
  # Add Missing formats from nformats into formats
  formats <- dplyr::bind_rows(
    formats,
    nformats[!df_collapse(nformats[, 1:6]) %in% df_collapse(formats[, 1:6]),]
  )
  
  # Re-order formats before next step and reset ORDER
  formats <- formats %>%
    dplyr::arrange( VARIABLE, O_ORDER ) %>%
    dplyr::group_by( VARIABLE ) %>%
    dplyr::mutate(
      ORDER = dplyr::row_number()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select( -O_ORDER ) %>%
    as.data.frame()
  
  # Add O_VARIABLE
  formats <- formats %>%
    dplyr::mutate(
      O_VARIABLE = paste0('o_', VARIABLE)
    )
  
  # Update object
  names(odata) <- paste0( 'o_', names(odata) )
  xpdb$data$data[[.problem]] <- dplyr::bind_cols( data, odata )
  xpdb$formats <- formats
  xpdb$data$index[[.problem]] <- dplyr::bind_rows(
    xpdb$data$index[[.problem]],
    xpdb$data$index[[.problem]] %>% 
      dplyr::filter( col %in% fvars) %>% 
      mutate( col = paste0('o_', col) )
  )
  var_types <- c()
  for ( fvar in fvars ){
    if (get_var_type(xpdb, .problem, fvar) == 'catcov'){
      var_types <- c(
        var_types,
        paste0(fvar, ' = \'catcov\'')
      )
    }
  }
  var_types <- paste(var_types, collapse = ', ')
  eval(
    parse(
      text = sprintf(
        'xpdb <- set_var_types(xpdb, .problem = %s, %s)',
        .problem,
        var_types
      )
    )
  )
  
  as.xpdb(xpdb)
  
}
