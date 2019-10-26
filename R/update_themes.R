#' Create xpose theme
#'
#' @description Create an xpose theme. This function will update the theme of an
#'   xpdb object. All plots generated with this xpdb will automatically use the
#'   defined xpose (\code{xp_theme}) and ggplot2 (\code{gg_theme}) themes.
#'
#' @param xpdb An \code{xpdb} object.
#' @param gg_theme A complete ggplot2 theme object (e.g.
#'   \code{\link[ggplot2]{theme_classic}}), a function returning a complete
#'   ggplot2 theme, or a change to the current \code{gg_theme}.
#' @param xp_theme A complete xpose theme object (e.g.
#'   \code{\link{theme_xp_default}}) or a list of modifications to the current
#'   \code{xp_theme} (e.g. \code{list(point_color = 'red', line_linetype =
#'   'dashed')}).
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @examples
#' # Before default theme
#' dv_vs_ipred(xpdb_ex_pk, facets = 'SEX')
#'
#' # Updating the gg_theme and xp_theme
#' xpdb_ex_pk %>%
#'   update_themes(gg_theme = theme(legend.position = 'top'),
#'                 xp_theme = list(point_color = 'blue',
#'                                 line_color  = 'blue')) %>%
#'   dv_vs_ipred(facets = 'SEX')
#'
#' @export
update_themes <- function(xpdb     = NULL,
                          gg_theme = NULL,
                          xp_theme = NULL,
                          quiet) {
  # Check input
  check_xpdb(xpdb, check = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Replace/Update gg_theme
  if (is.function(gg_theme) || is.theme(gg_theme)) {
    if (is.function(gg_theme) || attr(gg_theme, 'complete')) {
      attr(gg_theme, 'theme') <- as.character(substitute(gg_theme)) 
      xpdb$gg_theme <- gg_theme
    } else if (is.function(xpdb$gg_theme)) {
      gg_theme_attr <- paste(attr(xpdb$gg_theme, 'theme'), '(modified)')
      xpdb$gg_theme <- do.call(xpdb$gg_theme, args = list()) + gg_theme
      attr(xpdb$gg_theme, 'theme') <- gg_theme_attr
    } else {
      attr(xpdb$gg_theme, 'theme') <- paste(attr(xpdb$gg_theme, 'theme'), '(modified)')
      xpdb$gg_theme <- xpdb$gg_theme + gg_theme
    }
  } else if (!is.null(gg_theme) & !is.theme(gg_theme)) {
    msg('`gg_theme` argument not used. Reason: invalid input.', quiet)
  }
  
  # Replace/Update xp_theme
  if (!is.null(xp_theme)) { 
    if (is.xpose.theme(xp_theme)) {
      attr(xp_theme, 'theme') <- as.character(substitute(xp_theme)) 
      xpdb$xp_theme <- xp_theme
      
    } else if (!is.null(names(xp_theme))) {
      for (x in seq_along(xp_theme)) {
        xpdb$xp_theme[[names(xp_theme[x])]] <- xp_theme[[x]] # Beware drops NULL elements
      }
      attr(xpdb$xp_theme, 'theme') <- paste(attr(xpdb$xp_theme, 'theme'), '(modified)')
    } else {
      msg('`xp_theme` argument not used. Reason: invalid input.', quiet)
    }
  }
  as.xpdb(xpdb)
}
