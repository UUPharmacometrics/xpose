#' Residuals plotted against population predictions
#'
#' @description Model residuals plotted against population predictions (PRED). 
#' 
#' The residuals can be one of:
#' \itemize{
#'   \item RES: model residuals
#'   \item WRES: weighted model residuals
#'   \item IWRES: individual weighted model residuals
#'   \item CWRES: conditional weighted model residuals
#'   \item EWRES/ECWRES: Monte Carlo based model residuals
#'   \item NPDE: Normalized prediction distribution error 
#' }
#' 
#' @inheritParams dv_vs_pred
#' @param res Type of residual to be used. Default is "CWRES".
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' # Standard residual
#' res_vs_pred(xpdb_ex_pk, res = c('IWRES', 'CWRES'))
#' 
#' # Absolute value of the residuals
#' absval_res_vs_pred(xpdb_ex_pk, res = 'CWRES')
#' 
#' @export
res_vs_pred <- function(xpdb,
                        mapping  = NULL,
                        res      = 'CWRES',
                        group    = 'ID',
                        type     = 'pls',
                        title    = '@y vs. @x | @run',
                        subtitle = 'Ofv: @ofv',
                        caption  = '@dir',
                        tag      = NULL,
                        log      = NULL,
                        guide    = TRUE,
                        facets,
                        .problem,
                        quiet,
                        ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (length(res) > 1) {
    if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                                 variable = 'variable')
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet),
                    tidy = TRUE, value_col = res)
    vars <- aes_c(aes(
      x = .data[[xp_var(xpdb, .problem, type = 'pred')$col]], 
      y = .data[["value"]]), mapping)
  } else {
    if (missing(facets)) facets <- xpdb$xp_theme$facets
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet))
    vars <- aes_c(aes(
      x = .data[[xp_var(xpdb, .problem, type = 'pred')$col]], 
      y = .data[[toupper(res)]]), mapping)
  }
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = opt, mapping = vars, 
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
                guide_slope = 0, ...)
}


#' @rdname res_vs_pred
#' @export
absval_res_vs_pred <- function(xpdb,
                               mapping  = NULL,
                               res      = 'CWRES',
                               group    = 'ID',
                               type     = 'pls',
                               title    = '@y vs. @x | @run',
                               subtitle = 'Ofv: @ofv',
                               caption  = '@dir',
                               tag      = NULL,
                               log      = NULL,
                               guide    = FALSE,
                               facets,
                               .problem,
                               quiet,
                               ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (length(res) > 1) {
    if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                                 variable = 'variable')
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet),
                    tidy = TRUE, value_col = res)
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'pred')$col]], 
                      y = abs(.data[["value"]])), mapping)
  } else {
    if (missing(facets)) facets <- xpdb$xp_theme$facets
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet))
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'pred')$col]], 
                      y =  abs(.data[[toupper(res)]])), mapping)
  }
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = opt, mapping = vars, 
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
                guide_slope = 0, ...)
}


#' Residuals plotted against individual predictions
#'
#' @description Model residuals plotted against individual predictions (IPRED). 
#' 
#' The residuals can be one of:
#' \itemize{
#'   \item RES: model residuals
#'   \item WRES: weighted model residuals
#'   \item IWRES: individual weighted model residuals
#'   \item CWRES: conditional weighted model residuals
#'   \item EWRES/ECWRES: Monte Carlo based model residuals
#'   \item NPDE: Normalized prediction distribution error 
#' }
#' 
#' @inheritParams dv_vs_pred
#' @param res Type of residual to be used. Default is "CWRES".
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Faceting
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' # Standard residual
#' res_vs_ipred(xpdb_ex_pk, res = c('IWRES', 'CWRES'))
#' 
#' # Absolute value of the residuals
#' absval_res_vs_ipred(xpdb_ex_pk, res = 'CWRES')
#' 
#' @export
res_vs_ipred <- function(xpdb,
                         mapping  = NULL,
                         res      = 'CWRES',
                         group    = 'ID',
                         type     = 'pls',
                         title    = '@y vs. @x | @run',
                         subtitle = 'Ofv: @ofv',
                         caption  = '@dir',
                         tag      = NULL,
                         log      = NULL,
                         guide    = TRUE,
                         facets,
                         .problem,
                         quiet,
                         ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (length(res) > 1) {
    if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                                 variable = 'variable')
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet),
                    tidy = TRUE, value_col = res)
    vars <- aes_c(aes(
      x = .data[[xp_var(xpdb, .problem, type = 'ipred')$col]], 
      y = .data[["value"]]), mapping)
  } else {
    if (missing(facets)) facets <- xpdb$xp_theme$facets
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet))
    vars <- aes_c(aes(
      x = .data[[xp_var(xpdb, .problem, type = 'pred')$col]], 
      y = .data[[toupper(res)]]), mapping)
  }
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = opt, mapping = vars, 
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
                guide_slope = 0, ...)
}


#' @rdname res_vs_pred
#' @export
absval_res_vs_ipred <- function(xpdb,
                                mapping  = NULL,
                                res      = 'CWRES',
                                group    = 'ID',
                                type     = 'pls',
                                title    = '@y vs. @x | @run',
                                subtitle = 'Ofv: @ofv',
                                caption  = '@dir',
                                tag      = NULL,
                                log      = NULL,
                                guide    = FALSE,
                                facets,
                                .problem,
                                quiet,
                                ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (length(res) > 1) {
    if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                                 variable = 'variable')
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet),
                    tidy = TRUE, value_col = res)
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'ipred')$col]], 
                      y = abs(.data[["value"]])), mapping)
  } else {
    if (missing(facets)) facets <- xpdb$xp_theme$facets
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet))
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'ipred')$col]], 
                      y =  abs(.data[[toupper(res)]])), mapping)
  }
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = opt, mapping = vars, 
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
                guide_slope = 0, ...)
}

#' Residuals plotted against the independent variable
#'
#' @description Model residuals plotted against the independent variable (IDV).
#' 
#' The residuals can be one of:
#' \itemize{
#'   \item RES: model residuals
#'   \item WRES: weighted model residuals
#'   \item CWRES: conditional weighted model residuals
#'   \item EWRES/ECWRES: Monte Carlo based model residuals
#'   \item NPDE: Normalized prediction distribution error 
#' }
#'
#' @inheritParams dv_vs_pred
#' @param res Type of residual to be used. Default is "CWRES".
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' # Standard residual
#' res_vs_idv(xpdb_ex_pk, res = c('IWRES', 'CWRES'))
#' 
#' # Absolute value of the residuals
#' absval_res_vs_idv(xpdb_ex_pk, res = 'CWRES')
#' 
#' @export
res_vs_idv <- function(xpdb,
                       mapping  = NULL,
                       res      = 'CWRES',
                       group    = 'ID',
                       type     = 'pls',
                       title    = '@y vs. @x | @run',
                       subtitle = 'Ofv: @ofv',
                       caption  = '@dir',
                       tag      = NULL,
                       log      = NULL,
                       guide    = TRUE,
                       facets,
                       .problem,
                       quiet,
                       ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (length(res) > 1) {
    if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                                 variable = 'variable')
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet),
                    tidy = TRUE, value_col = res)
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'idv')$col]], 
                      y = .data[["value"]]), mapping)
  } else {
    if (missing(facets)) facets <- xpdb$xp_theme$facets
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet))
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'idv')$col]], 
                      y = .data[[toupper(res)]]), mapping)
  }
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = opt, mapping = vars, 
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log),
                yscale = check_scales('y', log),
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
                guide_slope = 0, ...)
}


#' @rdname res_vs_idv
#' @export
absval_res_vs_idv <- function(xpdb,
                              mapping  = NULL,
                              res      = 'CWRES',
                              group    = 'ID',
                              type     = 'pls',
                              title    = '@y vs. @x  | @run',
                              subtitle = 'Ofv: @ofv',
                              caption  = '@dir',
                              tag      = NULL,
                              log      = NULL,
                              guide    = FALSE,
                              facets,
                              .problem,
                              quiet,
                              ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (length(res) > 1) {
    if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                                 variable = 'variable')
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet),
                    tidy = TRUE, value_col = res)
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'idv')$col]], 
                      y = abs(.data[["value"]])), mapping)
  } else {
    if (missing(facets)) facets <- xpdb$xp_theme$facets
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet))
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'idv')$col]], 
                      y =  abs(.data[[toupper(res)]])), mapping)
  }
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = opt, mapping = vars,
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
                guide_slope = 0, ...)
}

#' Residuals plotted against the time after dose
#'
#' @description Model residuals plotted against the time after dose variable (IDV).
#'
#' The residuals can be one of:
#' \itemize{
#'   \item RES: model residuals
#'   \item WRES: weighted model residuals
#'   \item IWRES: individual weighted model residuals
#'   \item CWRES: conditional weighted model residuals
#'   \item EWRES/ECWRES: Monte Carlo based model residuals
#'   \item NPDE: Normalized prediction distribution error
#' }
#'
#' @inheritParams dv_vs_pred
#' @param res Type of residual to be used. Default is "CWRES".
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' # Standard residual
#' res_vs_tad(xpdb_ex_pk, res = c('IWRES', 'CWRES'))
#'
#' @export
res_vs_tad <- function(xpdb,
                       mapping  = NULL,
                       res      = 'CWRES',
                       tad      = c('TSPD', 'TSLD', 'TAD', 'TPD'),
                       group    = 'ID',
                       type     = 'pls',
                       title    = '@y vs. @x | @run',
                       subtitle = 'Ofv: @ofv',
                       caption  = '@dir',
                       tag      = NULL,
                       log      = NULL,
                       guide    = TRUE,
                       facets,
                       .problem,
                       quiet,
                       ...) {
  
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  
  # Change IDV variable type
  vars <- xpobj$data[xpobj$data$problem == .problem, ]$index[[1]]$col
  if ( length(tad) > 1 ) {
    tads <- c('TSPD', 'TSLD', 'TAD', 'TPD')
    tad <- tads[ tads %in% vars][1]
  }
  if ( !tad %in% vars ){
    stop(
      paste(tad, 'not present in data')
    )
  }
  xpdb <- set_var_types(xpdb = xpdb, .problem = .problem, idv = tad)
  
  # Get plot
  res_vs_idv(
    xpdb = xpdb,
    mapping = mapping,
    res = res,
    group = group,
    type = type,
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    log = log,
    guide = guide,
    facets = facets,
    .problem = .problem,
    quiet = quiet,
    ...
  )
}

#' @rdname res_vs_tad
#' @export
absval_res_vs_tad <- function(xpdb,
                              mapping  = NULL,
                              res      = 'CWRES',
                              group    = 'ID',
                              type     = 'pls',
                              title    = '@y vs. @x  | @run',
                              subtitle = 'Ofv: @ofv',
                              caption  = '@dir',
                              tag      = NULL,
                              log      = NULL,
                              guide    = FALSE,
                              facets,
                              .problem,
                              quiet,
                              ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- default_plot_problem(xpdb)
  check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  if (length(res) > 1) {
    if (missing(facets)) facets <- add_facet_var(facets = xpdb$xp_theme$facets, 
                                                 variable = 'variable')
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet),
                    tidy = TRUE, value_col = res)
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'tad')$col]], 
                      y = abs(.data[["value"]])), mapping)
  } else {
    if (missing(facets)) facets <- xpdb$xp_theme$facets
    opt <- data_opt(.problem = .problem, 
                    filter = only_obs(xpdb, .problem, quiet))
    vars <- aes_c(aes(x = .data[[xp_var(xpdb, .problem, type = 'tad')$col]], 
                      y =  abs(.data[[toupper(res)]])), mapping)
  }
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = opt, mapping = vars,
                type = type, guide = guide, facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
                guide_slope = 0, ...)
}
