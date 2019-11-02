#' @importFrom dplyr filter
#' @export
dplyr::filter


#' @importFrom dplyr slice
#' @export
dplyr::slice


#' @importFrom dplyr distinct
#' @export
dplyr::distinct

#' @importFrom dplyr mutate
#' @export
dplyr::mutate


#' @importFrom dplyr select
#' @export
dplyr::select


#' @importFrom dplyr rename
#' @export
dplyr::rename


#' @importFrom dplyr group_by
#' @export
dplyr::group_by


#' @importFrom dplyr ungroup
#' @export
dplyr::ungroup


#' @importFrom dplyr summarize
#' @export
dplyr::summarize


#' @importFrom dplyr summarise
#' @export
dplyr::summarise


#' @importFrom purrr %>%
#' @export
purrr::`%>%`


#' @importFrom ggforce facet_wrap_paginate
#' @export
ggforce::facet_wrap_paginate


#' @importFrom ggforce facet_grid_paginate
#' @export
ggforce::facet_grid_paginate


#' Test for xpose_theme class
#'
#' @description Reports whether x is an `xpose_theme` object
#'
#' @param x An object to be tested.
#'
#' @return Logical value, `TRUE` for `xpose_theme` class and `FALSE` otherwise.
#'
#' @keywords internal
#' @export
is.xpose.theme <- function(x) {
  inherits(x, 'xpose_theme')
}


#' Convert an object to `xpose_theme` class
#' 
#' @description Adds `xpose_theme` attribute to an object
#' 
#' @param x An object to be modified.
#' 
#' @return x with `xpose_theme` class.
#' 
#' @keywords internal
#' @export
as.xpose.theme <- function(x) {
  if (!is.xpose.theme(x)) {
    structure(x, class = c('xpose_theme', 'uneval'))
  } else {
    x
  }
}


#' Test for xpdb class
#' 
#' @description Reports whether x is an `xpdb` object
#' 
#' @param x An object to be tested.
#' 
#' @return Logical value, `TRUE` for `xpdb` class 
#' and `FALSE` otherwise.
#' 
#' @keywords internal
#' @export
is.xpdb <- function(x) {
  inherits(x, 'xpdb')
}


#' Convert an object to `xpdb` class
#' 
#' @description Adds `xpdb` attribute to an object
#' 
#' @param x An object to be modified.
#' 
#' @return x with `xpdb` class.
#' 
#' @keywords internal
#' @export
as.xpdb <- function(x) {
  if (!is.xpdb(x)) {
    structure(x, class = c('xpdb', 'uneval'))
  } else {
    x
  }
}


#' Test for nm_model class
#' 
#' @description Reports whether x is a `nm_model` object
#' 
#' @param x An object to be tested.
#' 
#' @return Logical value, `TRUE` for `nm_model` class 
#' and `FALSE` otherwise.
#' 
#' @keywords internal
#' @export
is.nm.model <- function(x) {
  inherits(x, 'nm_model')
}


#' Test for table_list_nm class
#' 
#' @description Reports whether x is a `table_list_nm` object
#' 
#' @param x An object to be tested.
#' 
#' @return Logical value, `TRUE` for `table_list_nm` class 
#' and `FALSE` otherwise.
#' 
#' @keywords internal
#' @export
is.table.list.nm <- function(x) {
  inherits(x, 'table_list_nm')
}


#' Convert an object to `table_list_nm` class
#' 
#' @description Adds `table_list_nm` attribute to an object
#' 
#' @param x An object to be modified.
#' 
#' @return x with `table_list_nm` class.
#' 
#' @keywords internal
#' @export
as.table.list.nm <- function(x) {
  if (!is.table.list.nm(x)) {
    structure(x, class = c('table_list_nm', class(x)))
  } else {
    x
  }
}


#' Test for xpose_plot class
#' 
#' @description Reports whether x is a `xpose_plot` object
#' 
#' @param x An object to be tested.
#' 
#' @return Logical value, `TRUE` for `xpose_plot` class 
#' and `FALSE` otherwise.
#' 
#' @keywords internal
#' @export
is.xpose.plot <- function(x) {
  inherits(x, 'xpose_plot')
}


#' Convert an object to `xpose_plot` class
#' 
#' @description Adds `xpose_plot` attribute to an object
#' 
#' @param x An object to be modified.
#' 
#' @return x with `xpose_plot` class.
#' 
#' @keywords internal
#' @export
as.xpose.plot <- function(x) {
  if (!is.xpose.plot(x)) {
    structure(x, class = c('xpose_plot', class(x)))
  } else {
    x
  }
}


#' Test for formula class
#' 
#' @description Reports whether x is a `formula` object
#' 
#' @param x An object to be tested.
#' 
#' @return Logical value, `TRUE` for `formula` class 
#' and `FALSE` otherwise.
#' 
#' @keywords internal
#' @export
is.formula <- function(x) {
  inherits(x, 'formula')
}


#' Time converter
#' 
#' @description Convert seconds to hh:mm:ss
#' 
#' @param x A string ('character' or 'numeric') of time in seconds.
#' 
#' @return A string of time in `hh:mm:ss`.
#' 
#' @keywords internal
#' @export
as.ctime <- function(x) {
  x <- round(as.numeric(x), 0)
  sprintf("%02d:%02d:%02d", 
          x %/% 3600,
          x %% 3600 %/% 60,
          x %% 60)
}


#' Message function
#' 
#' @description Message function with quiet option inspired from `ronkeizer/vpc`.
#' 
#' @param txt A string for the message.
#' @param quiet Should messages be displayed to the console.
#' 
#' @return Silent when quiet is `TRUE` or a message is quiet is `FALSE`.
#' 
#' @keywords internal
#' @export
msg <- function(txt, quiet = TRUE) {
  if (!quiet) message(txt)
}


#' Generate clean file paths
#' 
#' @description Wrapper around `file.path` that cleans trailing forward 
#' slash and missing `dir`.
#' 
#' @param dir A string or vector of strings containing the directory path.
#' @param file A file name or vector containing the file names.
#' 
#' @return A string or vector of string of the full file path.
#' 
#' @keywords internal
#' @export
file_path <- function(dir, file) {
  if (is.null(dir)) return(file) 
  
  # Remove trailing forward slash
  dir <- stringr::str_replace(dir, '\\/+$', '')
  file.path(dir, file)
}

#' Get file extension
#' 
#' @description Extract file extension from the filename string.
#' 
#' @param x A string or vector of strings containing the filenames with the extension.
#' @param dot Logical, if `TRUE` the returned value will contain the 
#' dot (e.g `.mod`) else only the extension itself will be returned (e.g. `mod`).
#' 
#' @return A string or vector of string of the file(s) extension.
#' 
#' @keywords internal
#' @export
get_extension <- function(x, dot = TRUE) {
  x <- stringr::str_extract(x, '\\.[[:alnum:]]+$')
  x[is.na(x)] <- ''
  if (!dot) x <- stringr::str_replace_all(x, '\\.', '')
  x
}


#' Generate extension string
#' 
#' @description Generate consistent extension strings by adding dot 
#' prefix whenever necessary.
#' 
#' @param x A string or vector of strings containing the extension to be standardized.
#' 
#' @return A string or vector of strings of extension(s).
#' 
#' @keywords internal
#' @export
make_extension <- function(x) {
  dplyr::if_else(!stringr::str_detect(x, '^\\..+'), stringr::str_c('.', x), x)
}


#' Update file extension
#'
#' @description Change the extension of a file.
#'
#' @param x A string or vector of strings containing the file name to be
#'   modified.
#' @param ext A string or vector of strings containing the name of the new
#'   extension(s).
#'
#' @return A string or vector of strings of file name(s).
#'
#' @keywords internal
#' @export
update_extension <- function(x, ext) {
  stringr::str_replace(x, '\\.[[:alnum:]]+$', ext)
}


#' Get software from the xpdb
#' 
#' @description Get the name of the modeling software on which the xpdb was created
#' 
#' @param xpdb An xpose database object.
#' 
#' @return The name of a modeling tool.
#' 
#' @keywords internal
#' @export
software <- function(xpdb) {
  x <- get_summary(xpdb)
  x <- x$value[x$label == 'software']
  if (is.null(x)) x <- 'na'
  unique(x)
}


#' Add `mod_num` and `mod_file` columns
#'
#' @description Add the model rank (`mod_num`) and model file name (`mod_file`)
#'   to a data frame.
#'
#' @param x A data frame.
#' @param mod_num The model rank.
#' @param mod_file A label for the model.
#'
#' @return The original dataframe preceded by the columns model and label.
#'
#' @keywords internal
#' @export
label_df <- function(x, mod_num, mod_file) {
  if (!is.null(x)) { 
    x %>% 
      dplyr::mutate(mod_num = mod_num, mod_file = mod_file) %>% 
      dplyr::select_at(.vars = dplyr::vars('mod_num', 'mod_file', dplyr::everything()))
  }
}

