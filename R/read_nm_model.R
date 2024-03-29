#' NONMEM model file parser
#'
#' @description Parse NONMEM model files in R format
#' 
#' @inheritParams xpose_data
#' @inheritSection xpose_data File path generation
#'
#' @details 
#' A NONMEM model output file (i.e. .lst, .out or .res) should preferably be provided to \code{read_nm_model} to allow for a more extensive xpose 
#' summary. However in some cases these output files may not contain the model code, thus preventing xpose from identifying the associated output 
#' tables names. In such cases xpose will attempt to read the associated model file (i.e. .mod or .ctl) instead to find the model code. Note: it 
#' is important that between the naming convention between the NONMEM output and the model file remains consistent e.g. run001.lst should be 
#' associated with run001.mod.
#' 
#' @seealso \code{\link{xpose_data}}, \code{\link{read_nm_tables}}
#' @return A \code{\link[dplyr]{tibble}} of class \code{model} containing the following columns: 
#' \itemize{
#'  \item problem: a numeric identifier for the $PROBLEM associated with the code.
#'  \item level: a unique numeric identifier to each subroutine block associated with the code.
#'  \item subroutine: a character identifier named after the 3 first letters of the subroutine name e.g. '$THETA' and 
#'  '$TABLE' will become 'the' and 'tab' respectively. In addition all output from the .lst is labeled 'lst', the general nonmem 
#'  output e.g. NM-TRAN messages are labelled 'oth'. With priors thp, tpv, omp, opd, sip, spd abbreviations are given to the THETAP, 
#'  THETAPV, OMEGAP, etc.
#'  \item code: the code without comments or subroutine names e.g. '$THETA 0.5 ; TVCL' will return '0.5'.
#'  \item comment: the last comment of a record e.g. '0.5 ; Clearance (L/h) ; TVCL' will return 'TVCL'.
#' }
#' 
#' @examples
#' \dontrun{
#' # Using the `file` argument to import a model file:
#' nm_model <- read_nm_model(file = 'run001.lst', dir = 'models')
#' 
#' # Using the `runno` argument to import a model file:
#' nm_model <- read_nm_model(runno = '001', ext = '.lst', dir = 'models')
#' }
#' 
#' @export
read_nm_model <- function(runno     = NULL,
                          prefix    = 'run',
                          ext       = '.lst',
                          file      = NULL,
                          dir       = NULL,
                          check_ext = TRUE) {
  
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (!is.null(runno)) {
    ext       <- make_extension(ext)
    full_path <- file_path(dir, stringr::str_c(prefix, runno, ext))
  } else {
    ext       <- get_extension(file)
    full_path <- file_path(dir, file)
  }
  
  if (check_ext & !ext %in% c('.lst', '.out', '.res', '.mod', '.ctl')) {
    stop(
      paste(
        'NONMEM model file extension should be one of .lst, .out, .res, .mod or .ctl. If you want to use the',
        ext ,'extension anyway use `check_ext = FALSE`'), 
        call. = FALSE) 
  }
  
  if (!file.exists(full_path)) { 
    stop('Model file ', basename(full_path), ' not found.', call. = FALSE) 
  }
  
  # Check for readr lazy loading
  if (readr::should_read_lazy() & .Platform$OS.type == "windows") {
    warning("Using lazy loading in `readr` on Windows can cause unexpected behavior and is not recommended for `xpose`.", call. = FALSE)
  }
  
  model <- readr::read_lines(full_path)
  
  if (!any(stringr::str_detect(model, '^\\s*\\$PROB')) && ext %in% c('.lst', '.out', '.res')) {
    # Attempts to recover the model code from model file rather than in the nonmem output file
    full_path <- update_extension(full_path, c('.mod', '.ctl'))
    full_path <- full_path[file.exists(full_path)]
    
    if (any(file.exists(full_path))) {
      warning(c('No model code found in `', ext, '` NONMEM output file importing `', 
                get_extension(full_path)[1], '` instead.'), call. = FALSE)
      model <- readr::read_lines(full_path[1])
    }
  }
  
  # Return error if input is bad
  if (!any(stringr::str_detect(model, '^\\s*\\$PROB'))) {
    stop(basename(full_path), ' is not a NONMEM model.', call. = FALSE)
  }
  
  model <- dplyr::tibble(code = model) %>% 
    dplyr::filter(!stringr::str_detect(.$code, '^;[^;]*$|^$')) %>% 
    dplyr::mutate(code = stringr::str_replace_all(.$code, '\\t+|\\s{2,}', ' ')) %>% 
    dplyr::mutate(
      problem     = findInterval(seq_along(.$code), which(stringr::str_detect(.$code, '^\\s*\\$PROB'))),
      level       = findInterval(seq_along(.$code), which(stringr::str_detect(.$code, '^\\s*\\$.+'))),
      subroutine  = stringr::str_match(.$code, '^\\s*\\$(\\w+)')[, 2]) %>% 
    tidyr::fill(dplyr::one_of('subroutine'))
  
  # Generate abbreviated subroutine names
  special <- c('THETAI', 'THETAR', 'THETAP', 'THETAPV', 'OMEGAP', 'OMEGAPD', 'SIGMAP', 'SIGMAPD')
  match_special <- match(model$subroutine[model$subroutine %in% special], special)
  model$subroutine[model$subroutine %in% special] <- c('thi', 'thr', 'thp', 'tpv', 
                                                       'omp', 'opd', 'sip', 'spd')[match_special]
  model$subroutine <- stringr::str_extract(tolower(model$subroutine), '[a-z]{1,3}')
  
  # Format lst part
  if (any(stringr::str_detect(model$code, 'NM-TRAN MESSAGES'))) {
    lst_rows <- which(stringr::str_detect(model$code, 'NM-TRAN MESSAGES')):nrow(model)
    model[lst_rows,] <- model %>% 
      dplyr::slice(lst_rows) %>% 
      dplyr::mutate(problem = findInterval(seq_along(.$problem), 
                                           which(stringr::str_detect(.$code, '^\\s*PROBLEM NO\\.:\\s*\\d+$')))) %>% 
      dplyr::mutate(level = 1 + .$level[1] + .$problem,
                    subroutine = 'lst')
  }
  
  # Handle other special cases
  if (any(stringr::str_detect(model$code, '#CPUT'))) {
    cput_row <- which(stringr::str_detect(model$code, '#CPUT'))
    model[cput_row, 'problem'] <- 0
    model[cput_row:nrow(model), 'level'] <- model[cput_row:nrow(model), ]$level + 1
  }
  
  if (any(stringr::str_detect(model$code, 'Stop Time'))) {
    end_rows <- which(stringr::str_detect(model$code, 'Stop Time')):nrow(model)
    model[end_rows, 'problem'] <- 0
    model[end_rows, 'level'] <- model[end_rows[1], ]$level + 1
  }
  
  model[is.na(model$subroutine) | (model$problem == 0 & model$subroutine == 'lst'), 'subroutine'] <- 'oth'
  
  # Remove subroutine names from the code
  model$code <- stringr::str_replace(model$code, '^\\s*\\$\\w+\\s*', '')
  
  # Remove empty rows but $PROBLEM
  model <- model[!stringr::str_detect(model$code, '^(\\s|\\t)*$') | model$subroutine == 'pro', ]
  
  # Create comment column
  code_rows <- !model$subroutine %in% c('lst', 'oth') | model$level == 0
  model[code_rows, 'comment'] <- stringr::str_match(model[code_rows, ]$code, ';\\s*(.*)\\s*$')[, 2]
  model[code_rows, 'code'] <- stringr::str_replace(model[code_rows, ]$code, '\\s*;.*$', '')
  
  # Remove na values and output
  tidyr::replace_na(model, replace = list(code = '', comment = '')) %>% 
    dplyr::select(dplyr::one_of('problem', 'level', 'subroutine', 'code', 'comment')) %>% 
    dplyr::mutate(problem = as.integer(.$problem),
                  level   = as.integer(.$level)) %>% 
    structure(file     = basename(full_path),
              dir      = dirname(full_path),
              software = 'nonmem',
              class    = c('nm_model', class(.)))
}
