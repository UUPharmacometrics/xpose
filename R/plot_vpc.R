#' Visual predictive checks
#'
#' @description Generate visual predictive checks (VPC)
#' 
#' @param xpdb An xpose database object.
#' @param vpc_type Only used when multiple vpc data are present in the same xpdb. The type of 
#' vpc to be created. Can be one of can be one of: 'continuous', 'categorical', 
#' 'censored' or 'time-to-event'.
#' @param smooth Should the bins be smoothed (connect bin midpoints, default) or shown as rectangular boxes.
#' @param mapping List of aesthetics mappings to be used for the xpose plot 
#' (e.g. \code{point_color}).
#' @param type String setting the type of plot to be used. Can be points 'p',
#' line 'l', area 'a', rug 'r' and text 't' or any combination of the five.
#' @param facets Either a character string to use \link[ggplot2]{facet_wrap} 
#' or a formula to use \link[ggplot2]{facet_grid}.
#' @param title Plot title. Use \code{NULL} to remove.
#' @param subtitle Plot subtitle. Use \code{NULL} to remove.
#' @param caption Page caption. Use \code{NULL} to remove.
#' @param tag Plot identification tag. Use \code{NULL} to remove.
#' @param log String assigning logarithmic scale to axes, can be either '', 
#' 'x', y' or 'xy'.   
#' @param guide Enable guide display in vpc continuous (e.g. lloq and uloq lines).
#' @param area_fill Shaded areas filling color, should be a vector of 3 values (i.e. low, med, high).
#' @param line_linetype Lines linetype, should be a vector of 3 values (i.e. low, med, high).
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... any additional aesthetics.
#' 
#' @inheritParams update_themes
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
#'   \item text: options to \code{geom_text}
#'   \item guide: options to \code{geom_hline}
#'   \item xscale: options to \code{scale_x_continuous} or \code{scale_x_log10}
#'   \item yscale: options to \code{scale_y_continuous} or \code{scale_y_log10}
#' }
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{vpc_data}}
#' @examples
#' xpdb_ex_pk %>% 
#'  vpc_data(opt = vpc_opt(n_bins = 7)) %>% 
#'  vpc()
#' @export
vpc <- function(xpdb,
                vpc_type = NULL,
                mapping  = NULL,
                smooth   = TRUE,
                type     = 'alpr',
                title    = 'Visual predictive checks | @run',
                subtitle = 'Number of simulations: @vpcnsim, confidence interval: @vpcci%',
                caption  = '@vpcdir',
                tag      = NULL,
                log      = NULL,
                guide    = TRUE,
                gg_theme,
                xp_theme,
                facets,
                quiet,
                area_fill     = c('steelblue3', 'grey60', 'steelblue3'),
                line_linetype = c('93', 'solid', '93'),
                ...) {
  # Check input
  check_xpdb(xpdb, check = 'special')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Fetch data
  if (!any(xpdb$special$method == 'vpc')) { 
    stop('No VPC data available. Please refer to the function `vpc_data()` function.', call. = FALSE)
  } else if (sum(xpdb$special$method == 'vpc') > 1) {
    if (is.null(vpc_type)) {
      stop('Several VPC data are associated with this xpdb. Please use the argument `vpc_type`.', call. = FALSE)
    } else {
      vpc_type <- match.arg(vpc_type, choices = c('continuous', 'categorical', 'censored', 'time-to-event'))
      if (!vpc_type %in% xpdb$special[xpdb$special$method == 'vpc', ]$type) {
        stop(c('No data are available for ', vpc_type, ' VPC. Change `vpc_type` to one of: ', 
               stringr::str_c(xpdb$special[xpdb$special$method == 'vpc', ]$type, collapse = ', '), '.'), call. = FALSE)
      }
      vpc_dat  <- xpdb$special[xpdb$special$method == 'vpc' & xpdb$special$type == vpc_type, ]
    }
  } else {
    if (!is.null(vpc_type) && !stringr::str_detect(xpdb$special$type, vpc_type)) {
      stop(c('No data are available for ', vpc_type, ' VPC. Change `vpc_type` to ', 
             xpdb$special[xpdb$special$method == 'vpc', ]$type, '.'), call. = FALSE)
    }
    vpc_dat  <- xpdb$special[xpdb$special$method == 'vpc', ]
    vpc_type <- vpc_dat$type
  }
  vpc_prob <- vpc_dat$problem 
  vpc_dat  <- vpc_dat$data[[1]]
  
  # Check that all faceting variable are present vpc_dat
  if (missing(facets)) facets <- vpc_dat$facets
  if (is.formula(facets)) {
    stratify <- all.vars(facets)
  } else {
    stratify <- facets
  }
  if (!all(stratify %in% colnames(vpc_dat$vpc_dat) & 
           stratify %in% colnames(vpc_dat$aggr_obs))) {
    unique(c(stratify[!stratify %in% colnames(vpc_dat$vpc_dat)], 
             stratify[!stratify %in% colnames(vpc_dat$aggr_obs)])) %>% 
      stringr::str_c(collapse = ', ') %>% 
      {stop('Faceting variable: ', ., ' not found. Use `stratify` to add a stratification variable in vpc_data().', 
            call. = FALSE)}
  }
  
  # Check type
  check_plot_type(type, allowed = c('a', 'l', 'p', 'r', 't'))
  
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
  if (is.null(mapping)) mapping <- aes()
  xp <- ggplot(data = NULL, mapping) + gg_theme 
  
  # Add shadded areas
  if (stringr::str_detect(type, stringr::fixed('a', ignore_case = TRUE))) {
    if (smooth) {
      xp <- xp + xp_geoms(mapping  = aes_c(aes(area_x     = .data[["bin_mid"]], 
                                               area_ymin  = .data[["low"]],
                                               area_ymax  = .data[["up"]],
                                               area_group = .data[["group"]],
                                               area_fill  = .data[["Simulations"]]), mapping),
                          xp_theme  = xpdb$xp_theme,
                          name      = 'area',
                          ggfun     = 'geom_ribbon',
                          area_data = vpc_dat$vpc_dat,
                          ...)
    } else {
      if (vpc_dat$psn_bins) {
        warning('Using `smooth = FALSE` along with `psn_bins = TRUE` may yield to misaligned obs and sim data.',
                ' Check the output carefully or use `vpc_data(psn_bins = FALSE)`', call. = FALSE)
      }
      xp <- xp + xp_geoms(mapping  = aes_c(aes(area_xmin  = .data[["bin_min"]],
                                               area_xmax  = .data[["bin_max"]],
                                               area_ymin  = .data[["low"]],
                                               area_ymax  = .data[["up"]],
                                               area_group = .data[["group"]],
                                               area_fill  = .data[["Simulations"]]), mapping),
                          xp_theme  = xpdb$xp_theme,
                          name      = 'area',
                          ggfun     = 'geom_rect',
                          area_data = vpc_dat$vpc_dat,
                          ...)
    }
  }
  
  # Add lines
  if (stringr::str_detect(type, stringr::fixed('l', ignore_case = TRUE))) {
    xp <- xp + xp_geoms(mapping   = aes_c(aes(line_x = .data[["bin_mid"]],
                                              line_y = .data[["value"]],
                                              line_group = .data[["group"]],
                                              line_linetype = .data[["Observations"]]), mapping),
                        xp_theme  = xpdb$xp_theme,
                        name      = 'line',
                        ggfun     = 'geom_line',
                        line_data = vpc_dat$aggr_obs,
                        ...)
  }
  
  # Add points
  if (stringr::str_detect(type, stringr::fixed('p', ignore_case = TRUE))) {
    if (vpc_dat$type == 'continuous') {
      xp <- xp + xp_geoms(mapping    = aes_c(aes(point_x = .data[["idv"]],
                                                 point_y = .data[["dv"]]), mapping),
                          xp_theme   = xpdb$xp_theme,
                          name       = 'point',
                          ggfun      = 'geom_point',
                          point_data = vpc_dat$obs,
                          ...)
    } else {
      warning('Points (type = \'p\') can only be added with continuous VPC.', call. = FALSE)
    }
  }
  
  # Add text
  if (stringr::str_detect(type, stringr::fixed('t', ignore_case = TRUE))) {
    if (vpc_dat$type == 'continuous') {
      xp <- xp + xp_geoms(mapping   = aes_c(aes(text_x = .data[["idv"]],
                                                text_y = .data[["dv"]],
                                                text_label = .data[["id"]]), mapping),
                          xp_theme  = xpdb$xp_theme,
                          name      = 'text',
                          ggfun     = 'geom_text',
                          text_data = vpc_dat$obs,
                          ...)
    } else { 
      warning('Text (type = \'t\') can only be added with continuous VPC.', call. = FALSE)
    }
  }
  
  # Add guides
  if (guide && vpc_type == 'continuous' && (!is.null(vpc_dat$lloq) | !is.null(vpc_dat$uloq))) {
    xp <- xp + xp_geoms(xp_theme = xpdb$xp_theme,
                        name     = 'guide',
                        ggfun    = 'geom_hline',
                        guide_yintercept = purrr::flatten_dbl(vpc_dat[c('lloq','uloq')]),
                        ...)
  }
  
  # Define scales
  xp <- xp + 
    # labs(x = vpc_dat$obs_cols[['idv']], y = vpc_dat$obs_cols[['dv']]) +
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'xscale',
             ggfun    = stringr::str_c('scale_x_', check_scales('x', log)),
             ...) +
    xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'yscale',
             ggfun    = stringr::str_c('scale_y_', check_scales('y', log)),
             ...)
  
  # Add rug
  if (stringr::str_detect(type, stringr::fixed('r', ignore_case = TRUE))) {
    extra_arg <- list(...)
    if (!'rug_sides' %in% names(extra_arg)) extra_arg$rug_sides <- 't'
    xp <- xp + do.call('xp_geoms', 
                       c(extra_arg,
                         list(mapping  = aes_c(aes(rug_x = .data[["idv"]]), mapping),
                              xp_theme = xpdb$xp_theme,
                              name     = 'rug',
                              ggfun    = 'geom_rug',
                              rug_data =  vpc_dat$aggr_obs %>% 
                                dplyr::distinct(!!!rlang::syms(c('bin', stratify)), .keep_all = TRUE) %>% 
                                dplyr::filter(!is.na(.$bin)) %>% 
                                tidyr::gather(key = 'edges', value = 'idv', dplyr::one_of('bin_min', 'bin_max')) %>% 
                                dplyr::distinct(!!!rlang::syms(c(stratify, 'idv')), .keep_all = TRUE))
                       ))
  }
  
  # Define panels
  if (!is.null(facets)) {
    xp <- xp + xpose_panels(xp_theme = xpdb$xp_theme, 
                            extra_args = c(list(facets = facets), list(...)))
  }
  
  # Add labels
  xp <- xp + labs(title = title, subtitle = subtitle, caption = caption)
  
  if (utils::packageVersion('ggplot2') >= '3.0.0') {
    xp <- xp + labs(tag = tag)
  }
  
  if ( isTRUE(xpdb$options$use_labelunit) ){
    
    xvar <- vpc_dat$obs_cols[['idv']]
    yvar <- vpc_dat$obs_cols[['dv']]
    
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
  
  # Add limits whenever needed
  if (vpc_dat$type == 'categorical') xp <- xp + coord_cartesian(ylim = c(0, 1))
  
  # Add color scales
  xp <- xp + 
    scale_fill_manual(values = area_fill) +
    scale_linetype_manual(values = line_linetype)
  
  # Add metadata to plots
  xp$xpose <- dplyr::tibble(problem = vpc_prob, subprob = 0L, 
                            descr = c('VPC directory', 'Number of simulations for VPC', 
                                      'VPC confidence interval', 'VPC prediction interval', 
                                      'VPC lower limit of quantification', 'VPC upper limit of quantification'),
                            label = c('vpcdir', 'vpcnsim', 'vpcci', 'vpcpi', 'vpclloq', 'vpculoq'),
                            value = c(vpc_dat$vpc_dir, vpc_dat$nsim, 
                                      100*diff(vpc_dat$opt$ci), 100*diff(vpc_dat$opt$pi),
                                      ifelse(is.null(vpc_dat$lloq), 'na', vpc_dat$lloq),
                                      ifelse(is.null(vpc_dat$uloq), 'na', vpc_dat$uloq))) %>% 
    dplyr::bind_rows(xpdb$summary) %>% 
    {list(fun = stringr::str_c('vpc_', vpc_dat$type),
          summary  = .,
          problem  = vpc_prob,
          quiet    = quiet,
          xp_theme = xpdb$xp_theme[stringr::str_c(c('title', 'subtitle', 
                                                    'caption', 'tag'), '_suffix')])}
  
  # Ouptut the plot
  as.xpose.plot(xp)
}  
