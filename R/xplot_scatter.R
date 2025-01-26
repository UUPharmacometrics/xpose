#' Default xpose scatter plot function
#'
#' @description Manually generate scatter plots from an xpdb object.
#'
#' @param xpdb An \code{xpose_data} object generated with \code{\link{xpose_data}}.
#' @param mapping List of aesthetics mappings to be used for the xpose plot 
#' (e.g. \code{point_color}).
#' @param group Grouping variable to be used for lines.
#' @param type String setting the type of plot to be used. Can be line 'l', 
#' point 'p', smooth 's' and text 't' or any combination of the four.
#' @param guide Should the guide (e.g. unity line) be displayed.
#' @param xscale Scale type for x axis (e.g. 'continuous', 'discrete', 'log10').
#' @param yscale Scale type for y axis (e.g. 'continuous', 'discrete', 'log10').
#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param tag Plot identification tag. Use \code{NULL} to remove.
#' @param plot_name Name to be used by \code{xpose_save()} when saving the plot.
#' @param opt A list of options in order to create appropriate data input for 
#' ggplot2. For more information see \code{\link{data_opt}}.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... Any additional aesthetics.
#' 
#' @inheritParams update_themes
#' 
#' @section Layers mapping:
#' Plots can be customized by mapping arguments to specific layers. The naming convention is 
#' layer_option where layer is one of the names defined in the list below and option is 
#' any option supported by this layer e.g. point_color = 'blue', smooth_method = 'lm', etc.
#' \itemize{
#'   \item point: options to \code{geom_point}
#'   \item line: options to \code{geom_line}
#'   \item guide: options to \code{geom_abline}
#'   \item smooth: options to \code{geom_smooth}
#'   \item text: options to \code{geom_text}
#'   \item xscale: options to \code{scale_x_continuous} or \code{scale_x_log10}
#'   \item yscale: options to \code{scale_y_continuous} or \code{scale_y_log10}
#' }
#' 
#' @section Faceting:
#' Every xpose plot function has built-in faceting functionalities. Faceting arguments 
#' are passed to the functions \code{\link[ggforce]{facet_wrap_paginate}} when the \code{facets} 
#' argument is a character string (e.g. \code{facets = c('SEX', 'MED1')}) or 
#' \code{\link[ggforce]{facet_grid_paginate}} when facets is a formula (e.g. \code{facets = SEX~MED1}).
#' All xpose plot functions accept all the arguments for the \code{\link[ggforce]{facet_wrap_paginate}} 
#' and \code{\link[ggforce]{facet_grid_paginate}} functions e.g. \code{dv_vs_ipred(xpdb_ex_pk, 
#' facets = SEX~MED1, ncol = 3, nrow = 3, page = 1, margins = TRUE, labeller = 'label_both')}.
#' 
#' Faceting options can either be defined in plot functions (e.g. \code{dv_vs_ipred(xpdb_ex_pk, 
#' facets = 'SEX')}) or assigned globally to an xpdb object via the \code{xp_theme} (e.g. \code{xpdb 
#' <- update_themes(xpdb_ex_pk, xp_theme = list(facets = 'SEX'))}). In the latter example all plots 
#' generate from this xpdb will automatically be stratified by `SEX`.
#' 
#' By default, some plot functions use a custom stratifying variable named `variable`, e.g. 
#' \code{eta_distrib()}. When using the \code{facets} argument, `variable` needs to be added manually 
#' e.g. \code{facets = c('SEX', 'variable')} or \code{facets = c('SEX', 'variable')}, but is optional, 
#' when using the \code{facets} argument in \code{xp_theme} variable is automatically added whenever needed.
#' 
#' @section Template titles:
#' Template titles can be used to create highly informative diagnostics plots. 
#' They can be applied to any plot title, subtitle, caption and tag. Template titles 
#' are defined via a single string containing key variables staring with a `@` (e.g. `@ofv`)
#' which will be replaced by their actual value when rendering the plot.
#' For example `'@run, @nobs observations in @nind subjects'` would become 
#' `'run001, 1022 observations in 74 subjects'`. The available key variables 
#' are listed under \code{\link{template_titles}}.
#' 
#' @seealso \code{\link{xplot_distrib}} \code{\link{xplot_qq}}
#' @examples
#' xplot_scatter(xpdb_ex_pk, aes(x = IPRED, y = DV))
#' 
#' @export
xplot_scatter <- function(xpdb,
                          mapping   = NULL,
                          group     = 'ID',
                          type      = 'pls',
                          guide     = FALSE,
                          xscale    = 'continuous',
                          yscale    = 'continuous',
                          title     = NULL,
                          subtitle  = NULL,
                          caption   = NULL,
                          tag       = NULL,
                          plot_name = 'scatter_plot',
                          gg_theme,
                          xp_theme,
                          opt,
                          quiet,
                          ...) {
  # Check input
  check_xpdb(xpdb, check = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Fetch data
  if (missing(opt)) opt <- data_opt()
  data <- fetch_data(xpdb, quiet = quiet, .problem = opt$problem, .subprob = opt$subprob,
                     .method = opt$method, .source = opt$source, simtab = opt$simtab, 
                     filter = opt$filter, tidy = opt$tidy, index_col = opt$index_col, 
                     value_col = opt$value_col, post_processing = opt$post_processing)
  if (is.null(data) || nrow(data) == 0) {
    stop('No data available for plotting. Please check the variable mapping and filering options.', 
         call. = FALSE)
  }
  
  # Check type
  check_plot_type(type, allowed = c('l', 'p', 's', 't'))
  
  # Assign xp_theme
  if (!missing(xp_theme)) xpdb <- update_themes(xpdb = xpdb, xp_theme = xp_theme)
  
  # Assign gg_theme
  if (missing(gg_theme)) {
    gg_theme <- xpdb$gg_theme
  } else {
    gg_theme <- update_themes(xpdb = xpdb, gg_theme = gg_theme)$gg_theme 
  }
  if (is.function(gg_theme)) {
    gg_theme <- do.call(gg_theme, args = list())
  }
  
  # Create ggplot base
  xp <- ggplot(data = data, mapping) + gg_theme 
  
  # Add lines
  if (stringr::str_detect(type, stringr::fixed('l', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = c(mapping, aes(line_group = .data[[group]])),
                        xp_theme = xpdb$xp_theme,
                        name     = 'line',
                        ggfun    = 'geom_line',
                        ...)
  }
  
  # Add points
  if (stringr::str_detect(type, stringr::fixed('p', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = mapping,
                        xp_theme = xpdb$xp_theme,
                        name     = 'point',
                        ggfun    = 'geom_point',
                        ...)
  }
  
  # Add text
  if (stringr::str_detect(type, stringr::fixed('t', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = c(mapping, aes(text_label = .data[[group]])),
                        xp_theme = xpdb$xp_theme,
                        name     = 'text',
                        ggfun    = 'geom_text',
                        ...)
  }
  
  # Add unity line
  if (guide) {
    xp <- xp + xp_geoms(xp_theme = xpdb$xp_theme,
                        name     = 'guide',
                        ggfun    = 'geom_abline',
                        ...)
  }
  
  # Add smooth
  if (stringr::str_detect(type, stringr::fixed('s', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping  = mapping,
                        xp_theme = xpdb$xp_theme,
                        name     = 'smooth',
                        ggfun    = 'geom_smooth',
                        ...)
  }
  
  # Define scales
  xp <- xp + 
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'xscale',
             ggfun    = paste0('scale_x_', xscale),
             ...) +
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'yscale',
             ggfun    = paste0('scale_y_', yscale),
             ...)
  
  # Define panels
  if (!is.null(list(...)[['facets']])) {
    xp <- xp + xpose_panels(xp_theme = xpdb$xp_theme, 
                            extra_args = list(...))
  }
  
  # Add labels
  xp <- xp + labs(title = title, subtitle = subtitle, caption = caption)
  
  if (utils::packageVersion('ggplot2') >= '3.0.0') {
    xp <- xp + labs(tag = tag)
  }
  
  if ( isTRUE(xpdb$options$use_labelunit) ){
    
    xvar <- get_aes(mapping, 'x')
    yvar <- get_aes(mapping, 'y')
    
    if ( !is.null(xvar) && xvar != '' ){
      xp  <- xp + xlab(
        get_var_labels_units( xpdb = xpdb, xvar )
      )
    }
    if ( !is.null(yvar) && yvar != '' ){
      xp  <- xp + ylab(
        get_var_labels_units( xpdb = xpdb, yvar )
      )
    }
  }
  
  # Add metadata to plots
  xp$xpose <- list(fun      = plot_name,
                   summary  = xpdb$summary,
                   problem  = attr(data, 'problem'),
                   subprob  = attr(data, 'subprob'),
                   method   = attr(data, 'method'),
                   quiet    = quiet,
                   xp_theme = xpdb$xp_theme[stringr::str_c(c('title', 'subtitle',
                                                             'caption', 'tag'), '_suffix')])
  
  # Output the plot
  as.xpose.plot(xp)
}
