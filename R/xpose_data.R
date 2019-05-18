#' Import NONMEM output into R
#'
#' @description Gather model outputs into a R database
#'
#' @param runno Run number to be used to generate model file name. Used in combination with \code{prefix} and \code{ext}.
#' @param prefix Prefix to be used to generate model file name. Used in combination with \code{runno} and \code{ext}.
#' @param ext Extension to be used to generate model file name.Should be one of '.lst' (default), '.out', '.res', '.mod' or '.ctl' for NONMEM.
#' @param file Model file name (preferably a '.lst' file) containing the file extension. Alternative to \code{prefix},
#' \code{runno} and \code{ext} arguments.
#' @param dir Location of the model files.
#' @param gg_theme A ggplot2 theme object (e.g. \code{\link[ggplot2]{theme_classic}}).
#' @param xp_theme An xpose theme or vector of modifications to the xpose theme
#' (e.g. \code{c(point_color = 'red', line_linetype = 'dashed')}).
#' @param simtab If \code{TRUE} only reads in simulation tables, if \code{FALSE} only reads estimation tables. 
#' Default \code{NULL} reads all tables. Option not compatible with manual_import.
#' @param manual_import If \code{NULL} (default) the names of the output tables to import will be obtained from the model file. 
#' To manually import files as in previous versions of xpose, the check the function \code{\link{manual_nm_import}}.
#' @param ignore Character vector be used to ignore the import/generation of: 'data', 'files', 'summary' or any
#' combination of the three.
#' @param extra_files A vector of additional output file extensions to be imported. Default is '.ext', '.cov', '.cor', '.phi', 
#' ".grd" for NONMEM.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... Additional arguments to be passed to the \code{\link{read_nm_tables}} functions.
#' 
#' @section File path generation:
#' The rules for model file names generation are as follow:
#' \itemize{
#'   \item with \code{runno}: the full path is generated as \code{<dir>/<prefix><runno>.<ext>} e.g. with \code{dir = 'model/pk'}, \code{prefix = 'run'}, \code{runno = '001'}, 
#'   \code{ext = '.lst'} the resulting path would be \code{model/pk/run001.lst}
#'   \item with \code{file}: the full path is generated as \code{<dir>/<file>} e.g. with \code{dir = 'model/pk'}, \code{file = 'run001.lst'} the resulting path
#'   would also be \code{model/pk/run001.lst}. Note: in this case the file extension should be provided as part of the `file` argument.
#'   }
#' 
#' @section Table format requirement:
#' When importing data, an \code{ID} column must be present in at least one table for each problem and for each `firstonly` 
#' category. \code{ID} columns are required to properly combine/merge tables and removing \code{NA} records. If 
#' \code{ID} columns are missing xpose will return the following warning: \code{Dropped `<tablenames>` due to missing 
#' required `ID` column.}.
#' 
#' @examples
#' \dontrun{
#' # Using the `file` argument to point to the model file:
#' xpdb <- xpose_data(file = 'run001.lst', dir = 'models')
#' 
#' # Using the `runno` argument to point to the model file:
#' xpdb <- xpose_data(runno = '001', ext = '.lst', dir = 'models')
#' 
#' # Using the `extra_files` argument to import specific output files only:
#' xpdb <- xpose_data(file = 'run001.lst', dir = 'models', extra_files = c('.ext', '.phi'))
#' 
#' # Using `ignore` to disable import of tables and output files:
#' xpdb <- xpose_data(file = 'run001.lst', dir = 'models', ignore = c('data', 'files'))
#' 
#' # Using `simtab` to disable import of simulation tables
#' xpdb <- xpose_data(file = 'run001.lst', dir = 'models', simtab = FALSE)
#' 
#' }
#' 
#' @export
xpose_data <- function(runno         = NULL,
                       prefix        = 'run',
                       ext           = '.lst',
                       file          = NULL,
                       dir           = NULL,
                       gg_theme      = theme_readable(),
                       xp_theme      = theme_xp_default(),
                       simtab        = NULL,
                       manual_import = NULL,
                       ignore        = NULL,
                       extra_files,
                       quiet,
                       ...) {
  # Check inputs
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (!is.theme(gg_theme) || !attr(gg_theme, 'complete')) {
    stop('Argument `gg_theme` must be a full ggplot2 theme. To modify a theme use update_themes() instead.', call. = FALSE) 
  }
  
  if (!is.xpose.theme(xp_theme)) {
   stop('Argument `xp_theme` must be a full xpose theme. To modify a theme use update_themes() instead.', call. = FALSE) 
  }
  
  if (missing(quiet)) quiet <- !interactive()
  
  # Check extensions
  if (!is.null(runno)) {
    ext <- make_extension(ext)
    full_path <- file_path(dir, stringr::str_c(prefix, runno, ext))
  } else {
    ext <- get_extension(file)
    if (ext == '') stop('An extension should be provided in the `file` name.', call. = FALSE)
    full_path <- file_path(dir, file)
  }
  
  # List tables
  if (ext %in% c('.lst', '.out', '.res', '.mod', '.ctl')) {
    software   <- 'nonmem'
    model_code <- read_nm_model(file = basename(full_path), 
                                dir  = dirname(full_path))
    
    if (is.null(manual_import)) {
      tbl_names <- list_nm_tables(model_code)
    } else {
      tbl_names <- list_nm_tables_manual(runno = runno, file = basename(full_path), 
                                         dir = dirname(full_path), tab_list = manual_import)
    }
  } else {
    stop('Model file currently not supported by xpose.', call. = FALSE)
  }  
  
  # Import estimation tables
  if ('data' %in% ignore) {
    msg('Ignoring data import.', quiet)
    data <- NULL
  } else if (software == 'nonmem') {
    data <- tryCatch(read_nm_tables(file = tbl_names, dir = NULL, 
                                    quiet = quiet, simtab = simtab, ...), 
                     error = function(e) {
                       warning(e$message, call. = FALSE)
                       return()
                     })
  }
  
  # Generate model summary
  if ('summary' %in% ignore) {
    msg('Ignoring summary generation', quiet)
    summary <- NULL
  } else if (software == 'nonmem') {
    summary <- tryCatch(summarise_nm_model(file = full_path, model = model_code, 
                                           software = software, rounding = xp_theme$rounding),
                        error = function(e) {
                          warning(c('Failed to create run summary. ', e$message), call. = FALSE)
                          return()
                        })
  }
  
  # Import output files
  if ('files' %in% ignore) {
    msg('Ignoring output files import', quiet)
    out_files <- NULL
  } else if (software == 'nonmem') {
    if (missing(extra_files)) {
      extra_files <- c('.ext', '.cor', '.cov', '.phi', '.grd', '.shk')
    } else {
      extra_files <- make_extension(extra_files) 
    }
    out_files <- full_path %>% 
      basename() %>% 
      update_extension(ext = extra_files) %>% 
      {tryCatch(read_nm_files(file = ., dir = dirname(full_path), quiet = quiet), 
                error = function(e) {
                  warning(e$message, call. = FALSE)
                  return()
                })}
  }
  
  # Label themes
  attr(gg_theme, 'theme') <- as.character(substitute(gg_theme)) 
  attr(xp_theme, 'theme') <- as.character(substitute(xp_theme)) 
  
  # Output xpose_data
  list(code = model_code, summary = summary, data = data,
       files = out_files, gg_theme = gg_theme, xp_theme = xp_theme,
       options = list(dir = dirname(full_path), quiet = quiet, 
                      manual_import = manual_import)) %>% 
    structure(class = c('xpose_data', 'uneval'))
}
