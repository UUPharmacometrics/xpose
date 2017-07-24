#' Visual predictive checks
#'
#' @description Generate visual predictive checks (VPC)
#' 
#' @param xpdb An xpose database object.
#' @param mapping List of aesthetics mappings to be used for the xpose plot 
#' (e.g. \code{point_color}).
#' @param type String setting the type of plot to be used points 'p',
#' line 'l', area 'a', rug 'r' and text 't' or any combination of the five.
#' @param smooth Should the bins be smoothed (connect bin midpoints, default) or shown as rectangular boxes.
#' @param facets Either a character string to use \link[ggplot2]{facet_wrap} 
#' or a formula to use \link[ggplot2]{facet_grid}.
#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param log String assigning logarithmic scale to axes, can be either '', 
#' 'x', y' or 'xy'.   
#' @param vpc_type Only used when multiple vpc data are present in the same xpdb. The type of 
#' vpc to be created. Can be one of can be one of: 'continuous', 'categorical', 
#' 'censored' or 'time-to-event'.
#' @param gg_theme A ggplot2 theme object (eg. \code{\link[ggplot2]{theme_classic}}).
#' @param xp_theme An xpose theme or vector of modifications to the xpose theme
#' (eg. \code{c(point_color = 'red', line_linetype = 'dashed')}).
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... any additional aesthetics.
#' 
#' @section Layers mapping:
#' Plots can be customized by mapping arguments to specific layers. The naming convention is 
#' layer_option where layer is one of the names defined in the list below and option is 
#' any option supported by this layer e.g. point_color = 'blue', area_fill = 'green', etc.
#' \itemize{
#'   \item point: options to \code{geom_point}
#'   \item line: options to \code{geom_line}
#'   \item area: options to \code{geom_ribbon} (smooth = TRUE) or \code{geom_rect} (smooth = FALSE)
#'   \item rug: options to \code{geom_rug}
#'   \item panel: options to \code{facet_wrap} (facets is character) or \code{facet_grid}
#'   (facets is a formula)
#'   \item smooth: options to \code{geom_smooth}
#'   \item text: options to \code{geom_text}
#'   \item xscale: options to \code{scale_x_continuous} or \code{scale_x_log10}
#'   \item yscale: options to \code{scale_y_continuous} or \code{scale_y_log10}
#' }
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{vpc_data}
#' @examples
#' xpdb_ex_pk %>% 
#'  vpc_data() %>% 
#'  vpc()
#' @export
vpc <- function(xpdb,
                mapping  = NULL,
                type     = 'alr',
                smooth   = TRUE,
                facets,
                title    = 'Visual predictive checks | @run',
                subtitle = NULL,
                caption  = '@dir',
                log      = NULL,
                vpc_type = NULL,
                gg_theme,
                xp_theme,
                quiet,
                ...) {
  # Check input
  check_xpdb(xpdb, check = 'special')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Fetch data
  if (!any(xpdb$special$method == 'vpc')) { 
    msg('No vpc data available. Please refer to the function `vpc_data()` function.', quiet)
    return()
  } else if (sum(xpdb$special$method == 'vpc') > 1) {
    if (is.null(vpc_type)) {
      msg('Several vpc data are associated with this xpdb. Please use the argument `vpc_type`.', quiet)
      return()
    } else {
      vpc_type <- match.arg(vpc_type, choices = c('continuous', 'categorical', 'censored', 'time-to-event'))
      if (!vpc_type %in% xpdb$special[xpdb$special$method == 'vpc', ]$type) {
        msg(c('No data are available for ', vpc_type, ' VPC. Change `vpc_type` to one of: ', 
              stringr::str_c(xpdb$special[xpdb$special$method == 'vpc', ]$type, collapse = ', '), '.'), quiet)
        return()
      }
      vpc_dat  <- xpdb$special[xpdb$special$method == 'vpc' & xpdb$special$type == vpc_type, ]$data[[1]]
    }
  } else {
    vpc_dat <- xpdb$special[xpdb$special$method == 'vpc', ]$data[[1]]
  }
  
  # Define panels
  if (missing(facets) && !is.null(vpc_dat$facets)) {
    facets <- vpc_dat$facets
  } else if (missing(facets) && vpc_dat$type == 'categorical') {
    facets <- 'group'
  } else if (missing(facets)) {
    facets <- NULL 
  }
  
  # Check for present of facets in vpc_dat
  stratify <- facets
  if (is.formula(stratify)) stratify <- all.vars(stratify)
  if (!all(stratify %in% colnames(vpc_dat$vpc_dat) & 
           stratify %in% colnames(vpc_dat$aggr_obs))) {
    unique(c(stratify[!stratify %in% colnames(vpc_dat$vpc_dat)], 
             stratify[!stratify %in% colnames(vpc_dat$aggr_obs)])) %>% 
      stringr::str_c(collapse = ', ') %>% 
      {stop('Faceting variable: ', ., ' not found. Use `stratify` to add a stratification variable in vpc_data().', 
          call. = FALSE)}
  }
  
  # Assing xp_theme and gg_theme
  if (!missing(xp_theme)) {
    xpdb <- update_themes(xpdb = xpdb, xp_theme = xp_theme)
  }
  if (missing(gg_theme)) {
    gg_theme <- xpdb$gg_theme
  }
  
  # Create ggplot base
  if (is.null(mapping)) mapping <- aes()
  xp <- ggplot(data = NULL, mapping, ...) + gg_theme 
  
  # Add shadded areas
  if (stringr::str_detect(type, stringr::fixed('a', ignore_case = TRUE))) {
    if (smooth) {
      xp <- xp + xp_geoms(mapping  = aes_c(aes_string(area_x = 'bin_mid', 
                                                      area_ymin = 'low',
                                                      area_ymax = 'up',
                                                      area_group = 'group',
                                                      area_fill = 'Simulations'), mapping),
                          xp_theme = xpdb$xp_theme,
                          name     = 'area',
                          ggfun    = 'geom_ribbon',
                          area_data = vpc_dat$vpc_dat,
                          ...)
    } else {
      xp <- xp + xp_geoms(mapping  = aes_c(aes_string(area_xmin = 'bin_min',
                                                      area_xmax = 'bin_max',
                                                      area_ymin = 'low',
                                                      area_ymax = 'up',
                                                      area_group = 'group',
                                                      area_fill = 'Simulations'), mapping),
                          xp_theme = xpdb$xp_theme,
                          name     = 'area',
                          ggfun    = 'geom_rect',
                          area_data = vpc_dat$vpc_dat,
                          ...)
    }
  }
  
  # Add lines
  if (stringr::str_detect(type, stringr::fixed('l', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = aes_c(aes_string(line_x = 'bin_mid',
                                                    line_y = 'value',
                                                    line_group = 'group',
                                                    line_linetype = 'Observations'), mapping),
                        xp_theme = xpdb$xp_theme,
                        name     = 'line',
                        ggfun    = 'geom_line',
                        line_data = vpc_dat$aggr_obs,
                        ...)
  }
  
  # Add points
  if (stringr::str_detect(type, stringr::fixed('p', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = aes_c(aes_string(point_x = 'idv',
                                                    point_y = 'dv'), mapping),
                        xp_theme = xpdb$xp_theme,
                        name     = 'point',
                        ggfun    = 'geom_point',
                        point_data = vpc_dat$obs,
                        ...)
  }
  
  # Add text
  if (stringr::str_detect(type, stringr::fixed('t', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = aes_c(aes_string(text_x = 'idv',
                                                    text_y = 'dv',
                                                    text_label = 'id'), mapping),
                        xp_theme = xpdb$xp_theme,
                        name     = 'text',
                        ggfun    = 'geom_text',
                        text_data = vpc_dat$obs,
                        ...)
  }
  
  # Define scales
  xp <- xp + 
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'xscale',
             ggfun    = stringr::str_c('scale_x_', check_scales('x', log)),
             xscale_name = vpc_dat$obs_cols[['idv']],
             ...) +
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'yscale',
             ggfun    = stringr::str_c('scale_y_', check_scales('y', log)),
             yscale_name = vpc_dat$obs_cols[['dv']],
             ...)
  
  if (!is.null(facets)) {
    if (!is.formula(facets)) {
      xp <- xp + xp_geoms(mapping  = mapping,
                          xp_theme = xpdb$xp_theme,
                          name     = 'panel',
                          ggfun    = 'facet_wrap_paginate',
                          panel_facets = facets,
                          ...)
    } else {
      xp <- xp + xp_geoms(mapping  = mapping,
                          xp_theme = filter_xp_theme(xpdb$xp_theme, 'panel_dir', 'drop'),
                          name     = 'panel',
                          ggfun    = 'facet_grid_paginate',
                          panel_facets = facets,
                          ...)
    }
  }
  
  # Add rug
  if (stringr::str_detect(type, stringr::fixed('r', ignore_case = TRUE))) {
    if (is.formula(facets)) {
      facets_string <- all.vars(facets)
    } else {
      facets_string <- facets
    }
    xp <- xp + xp_geoms(mapping  = aes_c(aes_string(rug_x = 'idv'), mapping),
                        xp_theme = xpdb$xp_theme,
                        name     = 'rug',
                        ggfun    = 'geom_rug',
                        rug_data =  vpc_dat$aggr_obs %>% 
                          dplyr::distinct_(.dots = c('bin', facets_string), .keep_all = TRUE) %>% 
                          dplyr::filter(!is.na(.$bin)) %>% 
                          tidyr::gather(key = 'edges', value = 'idv', dplyr::one_of('bin_min', 'bin_max')) %>% 
                          dplyr::distinct_(.dots = c(facets_string, 'idv'), .keep_all = TRUE),
                        rug_sides = 't',
                        ...)
       
  }

  # Add labels
  xp <- xp + labs(title = title, subtitle = subtitle, caption = caption)
  
  # Add limits whenever needed
  if (vpc_dat$type == 'categorical') xp <- xp + coord_cartesian(ylim = c(0, 1))
  
  # Add color scales
  xp <- xp + 
    scale_fill_manual(values = c('deepskyblue3', 'grey60', 'deepskyblue3')) +
    scale_linetype_manual(values = c('93', 'solid', '93'))
  
  # Add metadata to plots
  xpdb_summary <- xpdb$summary
  if (!is.null(vpc_dat$psn_folder)) {
   xpdb_summary$value[xpdb_summary$label == 'dir'] <- stringr::str_c('VPC folder: ', vpc_dat$psn_folder)
  }
  xp$xpose <- list(fun      = stringr::str_c('vpc_', vpc_dat$type),
                   summary  = xpdb_summary,
                   problem  = dplyr::if_else(is.null(vpc_dat$psn_folder), 0, vpc_dat$sim_problem),
                   quiet    = quiet,
                   xp_theme = xpdb$xp_theme[stringr::str_c(c('title', 'subtitle', 'caption'), '_suffix')])
  
  # Ouptut the plot
  structure(xp, class = c('xpose_plot', class(xp)))
}  
