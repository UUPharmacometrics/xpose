#' Determine if a character string is a NONMEM reserved label
#'
#' @param x A character string (scalar or vector)
#' @return A logical vector the same length as \code{x} indicating if the string
#'   is a NONMEM reserved label.
is_nm_reserved_label <- function(x) {
  nm_reserved_labels <-
    c("ID", "L1", "L2", "DV", "MDV", "RAW_", "MRG_", "RPT_", "TIME", 
      "DATE", "DAT1", "DAT2", "DAT3", "DROP", "SKIP", "EVID", "AMT", 
      "RATE", "SS", "II", "ADDL", "CMT", "PCMT", "CALL", "CONT", "XVID1", 
      "XVID2", "XVID3", "XVID4", "XVID5")
  x %in% nm_reserved_labels
}

#' Extract the parameter name mapping from a NONMEM $INPUT block
#'
#' @param code A source of data for a NONMEM $INPUT block (methods are available
#'   for multiple sources)
#' @return A named character vector mapping NONMEM reserved labels to input
#'   data.
#' @seealso \code{\link{is_nm_reserved_label}}, \code{\link{read_nm_model}},
#'   \code{\link{read_nm_tables}}
#' @examples
#' parse_nm_input_record("FOO=ID")
#' parse_nm_input_record("FOO=ID BAR")
#' parse_nm_input_record(c("FOO=ID", "BAR"))
#' \dontrun{
#' # Read your model into xpdb
#' parse_nm_input_record(xpdb)
#' }
#' @export
parse_nm_input_record <- function(code) 
  UseMethod("parse_nm_input_record")

parse_nm_input_record.data.frame <- function(code) {
  if (ncol(code) == 1) {
    parse_nm_input_record(code[[1]])
  } else if (all(c("code", "subroutine") %in% names(code))) {
    parse_nm_input_record(
      code$code[code$subroutine %in% "inp"])
  } else {
    stop("Cannot interpret data.frame to parse NONMEM input block")
  }
}

parse_nm_input_record.xpose_data <- function(code) {
  parse_nm_input_record(code$code)
}

parse_nm_input_record.factor <- function(code) {
  parse_nm_input_record.character(as.character(code))
}
  
parse_nm_input_record.character <- function(code) {
  all_code <- paste(code, collapse=" ")
  var_blocks <- strsplit(x=all_code, split=" +")[[1]]
  name_blocks <- strsplit(x=var_blocks, split="=", fixed=TRUE)
  ret <-
    name_blocks %>%
    purrr::map(.f=function(x) {
      if (length(x) == 1) {
        setNames(x, nm=x)
      } else if (length(x) == 2) {
        if (is_nm_reserved_label(x[2])) {
          setNames(object=x[1], nm=x[2])
        } else {
          setNames(object=x[2], nm=x[1])
        }
      } else {
        stop("$INPUT elements must be mapped with either no equal sign or one equal sign.  Cannot parse '", paste(x, collapse="="), "'")
      }
    }) %>%
    unlist()
  if (is.null(ret)) {
    # Give back non-null
    character(0)
  } else {
    ret
  }
}

