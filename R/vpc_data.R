#' Visual predictive checks data
#'
#' @description Generate visual predictive checks (VPC) data
#' 
#' @param xpdb An xpose database object.
#' @param opt A list of options regarding binning, pi and ci computation. 
#' For more information see \code{\link{vpc_opt}}.
#' @param stratify Either a character string or a formula to stratify the data. For 'categorical' vpcs the 
#' stratification fixed to the different categories.
#' @param vpc_type A string specifying the type of VPC to be created, can be one of: 
#' 'continuous', 'categorical', 'censored' or 'time-to-event'.
#' @param psn_folder Specify a PsN-generated VPC-folder.
#' @param psn_bins Only used with argument \code{psn_folder}. If \code{TRUE} bins will be inputed from the 
#' PsN vpc_bins.txt file. If \code{FALSE} (default) bins will be re-calculated in R. Note that when 
#' \code{psn_bins = TRUE} only the first bin array will be used and applied to all panels as it is not 
#' currently possible to define per panel binning in xpose. In addition when \code{psn_bins = TRUE} is 
#' used along with \code{vpc(smooth = FALSE)} the observations lines may not be centered in the bins. 
#' Check the output carefully.
#' @param obs_problem Alternative to the option `psn_folder`. The $problem number to 
#' be used for observations. By default returns the last estimation problem.
#' @param sim_problem Alternative to the option `psn_folder`. The $problem number to 
#' be used for simulations. By default returns the last simulation problem.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' 
#' @seealso \code{\link{vpc}} \code{\link{vpc_opt}}
#' @examples
#' \dontrun{
#' xpdb_ex_pk %>% 
#'  vpc_data() %>% 
#'  vpc()
#' }
#' @export
vpc_data <- function(xpdb,
                     opt,
                     stratify,
                     vpc_type    = c('continuous', 'categorical', 'censored', 'time-to-event'),
                     psn_folder  = NULL,
                     psn_bins    = FALSE,
                     obs_problem = NULL,
                     sim_problem = NULL,
                     quiet) {
  # Check input
  check_xpdb(xpdb, check = ifelse(is.null(psn_folder), 'data', FALSE))
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(opt)) opt <- vpc_opt()
  vpc_type <- match.arg(arg = vpc_type, choices = c('continuous', 'categorical', 
                                                    'censored', 'time-to-event'))
  
  # Get raw data
  msg(c('\nVPC ', vpc_type, ' ', rep('-', 30 - nchar(vpc_type)), 
        '\n1. Gathering data & settings'), quiet)
  if (is.null(psn_folder)) {
    # When using xpdb tables
    if (is.null(obs_problem)) obs_problem <- last_data_problem(xpdb, simtab = FALSE)
    if (is.null(sim_problem)) sim_problem <- last_data_problem(xpdb, simtab = TRUE)
    msg(c('Using xpdb simulation problem ', ifelse(is.na(sim_problem), '<na>', sim_problem), 
          ' and observation problem ', ifelse(is.na(obs_problem), '<na>', obs_problem), '.'), quiet)
    obs_data <- get_data(xpdb, .problem = obs_problem) 
    sim_data <- get_data(xpdb, .problem = sim_problem)
    if (is.null(opt$obs_cols)) {
      obs_cols <- xp_var(xpdb, obs_problem, type = c('id', 'idv', 'dv', 'pred'))
      obs_cols <- purrr::set_names(as.list(obs_cols$col), nm = obs_cols$type)
    }
    if (is.null(opt$sim_cols)) {
      sim_cols <- xp_var(xpdb, sim_problem, type = c('id', 'idv', 'dv', 'pred'))
      sim_cols <- purrr::set_names(as.list(sim_cols$col), nm = sim_cols$type)
    }
    vpc_nsim <- xpdb %>% 
      get_summary(sim_problem) %>% 
      {dplyr::filter(.data = ., .$label == 'nsim')$value}
  } else {
    # When using PsN
    parsed_psn_vpc <- psn_vpc_parser(xpdb = xpdb, psn_folder = psn_folder, 
                                     psn_bins = psn_bins, opt = opt, quiet = quiet)
    obs_data   <- parsed_psn_vpc$obs_data
    obs_cols   <- parsed_psn_vpc$obs_cols
    sim_data   <- parsed_psn_vpc$sim_data
    sim_cols   <- parsed_psn_vpc$sim_cols
    opt        <- parsed_psn_vpc$opt
    psn_folder <- parsed_psn_vpc$psn_folder
    vpc_nsim   <- parsed_psn_vpc$nsim
  }
  
  if (is.null(obs_data) && is.null(sim_data)) {
    stop('No data table found.', call. = FALSE)
  }
  
  # Get info on stratification
  if (missing(stratify)) {
    if (is.null(psn_folder)) { 
      stratify <- NULL 
    } else {
      stratify <- get_psn_vpc_strat(parsed_psn_vpc$psn_cmd)
    }
  }
  if (vpc_type == 'categorical') stratify <- add_facet_var(stratify, 'group')
  facets <- stratify
  if (is.formula(stratify)) stratify <- all.vars(stratify)
  if (!is.null(stratify)) {
    msg(c('Setting stratifying variable to ', stringr::str_c(stratify, collapse = ', ')), quiet)
  }
  
  # Messages for lloq and uloq
  if (!is.null(opt$lloq)) msg(c('Setting lloq to ', opt$lloq, '.'), quiet)
  if (!is.null(opt$uloq)) msg(c('Setting uloq to ', opt$uloq, '.'), quiet)
  
  # Generate vpc data
  msg('\n2. Computing VPC data', quiet)
  if (vpc_type == 'continuous') {
    vpc_dat <- vpc::vpc(obs = obs_data, sim = sim_data, psn_folder = NULL, bins = opt$bins, 
                        n_bins = opt$n_bins, bin_mid = opt$bin_mid, obs_cols = obs_cols, 
                        sim_cols = sim_cols, stratify = stratify, pred_corr = opt$pred_corr, 
                        pred_corr_lower_bnd = opt$pred_corr_lower_bnd, pi = opt$pi, ci = opt$ci, 
                        uloq = opt$uloq, lloq = opt$lloq, smooth = FALSE, vpcdb = TRUE, verbose = !quiet) 
  } else if (vpc_type == 'categorical') {
    vpc_dat <- vpc::vpc_cat(obs = obs_data, sim = sim_data, psn_folder = NULL, bins = opt$bins, 
                            n_bins = opt$n_bins, bin_mid = opt$bin_mid, obs_cols = obs_cols, 
                            sim_cols = sim_cols, ci = opt$ci, uloq = opt$uloq, lloq = opt$lloq, 
                            smooth = FALSE, vpcdb = TRUE, verbose = !quiet) 
  } else if (vpc_type == 'censored') {
    
    # TEMPORARY check compatibility of versions
    if (utils::packageVersion("dplyr") > "0.8.5" & utils::packageVersion("vpc") < "1.2.1") {
      stop(paste0("`dplyr` (v", utils::packageVersion("dplyr"),  
                  ") and `vpc` (v", utils::packageVersion("vpc"), ") are not compatible with each other."), call. = FALSE)
    }
    
    vpc_dat <- vpc::vpc_cens(obs = obs_data, sim = sim_data, psn_folder = NULL, bins = opt$bins, 
                             n_bins = opt$n_bins, bin_mid = opt$bin_mid, obs_cols = obs_cols, 
                             sim_cols = sim_cols, stratify = stratify, ci = opt$ci, 
                             uloq = opt$uloq, lloq = opt$lloq, smooth = FALSE, vpcdb = TRUE, verbose = !quiet) 
  } else {
    stop('Time-to-event VPC are not yet available in xpose.', call. = FALSE)
    # vpc_dat <- vpc::vpc_tte(obs = obs_data, sim = sim_data, psn_folder = NULL, bins = opt$bins, 
    #                         n_bins = opt$n_bins, obs_cols = obs_cols, sim_cols = sim_cols, stratify = stratify, 
    #                         ci = opt$ci, smooth = FALSE, rtte = opt$rtte, rtte_calc_diff = opt$rtte_calc_diff, 
    #                         events = opt$events, kmmc = opt$kmmc, reverse_prob = opt$reverse_prob, 
    #                         as_percentage = opt$as_percentage, vpcdb = TRUE, verbose = !quiet) 
  } 
  
  # Assign a problem number to the vpc
  if (!is.null(xpdb$special$problem)) {
    if (!is.null(xpdb$special) && length(xpdb$special$problem[xpdb$special$method == 'vpc' & xpdb$special$type == vpc_type])) {
      vpc_prob <- xpdb$special$problem[xpdb$special$method == 'vpc' & xpdb$special$type == vpc_type] # Overwrite
    } else {
      vpc_prob <- max(xpdb$special$problem) + 1 # Add new problem
    }
  } else {
    vpc_prob <- ifelse(!is.null(xpdb$data$problem), max(xpdb$data$problem) + 1, 1) # Start new numbering
  }
  
  # Format vpc output
  xpdb$special <- vpc_dat %>%
    purrr::keep_at(at = c(
      'vpc_dat', 'smooth', 'stratify', 'aggr_obs', 'obs', 
      'bins', 'lloq', 'uloq', 'show', 'type', 'scales'
      )) %>%
    purrr::map_at('vpc_dat', function(x) {
      x <- x %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate_all(.funs = unname) %>% # remove warning in gather
        tidyr::gather(key = 'tmp', value = 'value', dplyr::matches('\\.(low|med|up)')) %>% 
        tidyr::separate(col = !!rlang::sym('tmp'), 
                        into = c('Simulations', 'ci'), sep = '\\.') %>% 
        tidyr::spread(key = 'ci', value = 'value') %>%
        dplyr::ungroup()
      
      if (vpc_type == 'continuous') {
        x <- dplyr::mutate(.data = x, 
                           Simulations = factor(x$Simulations, levels = c('q5', 'q50', 'q95'),
                                                labels = c(stringr::str_c(min(opt$pi)*100, 'th percentile'), 
                                                           'Median', stringr::str_c(max(opt$pi)*100, 'th percentile'))))
      } else {
        x <- dplyr::mutate(.data = x, 
                           Simulations = factor(x$Simulations, levels = 'q50', labels = 'Median'))
      }
      if ('strat2' %in% colnames(x)) {
        x$strat1 <- stringr::str_replace(x$strat1, stringr::str_c(vpc_dat$stratify[1], '='), '')
        x$strat2 <- stringr::str_replace(x$strat2, stringr::str_c(vpc_dat$stratify[2], '='), '')
        colnames(x)[colnames(x) == 'strat1'] <- vpc_dat$stratify[1]
        colnames(x)[colnames(x) == 'strat2'] <- vpc_dat$stratify[2]
      } else if (!is.null(vpc_dat$stratify)) {
        x[, vpc_dat$stratify] <- stringr::str_replace(x$strat, stringr::str_c(vpc_dat$stratify, '='), '')
      }
      dplyr::mutate(.data = x, group = as.numeric(interaction(x$strat, x$Simulations)))
    }) %>% 
    purrr::map_at('aggr_obs', function(x) {
      if (vpc_type == 'continuous') {
        x <- x %>% 
          dplyr::ungroup() %>%
          dplyr::mutate_all(.funs = unname) %>% # remove warning in gather
          tidyr::gather(key = 'Observations', value = 'value', dplyr::one_of('obs5', 'obs50', 'obs95')) %>% 
          dplyr::mutate(Observations = factor(.$Observations, levels = c('obs5', 'obs50', 'obs95'),
                                              labels = c(stringr::str_c(min(opt$pi)*100, 'th percentile'), 
                                                         'Median', stringr::str_c(max(opt$pi)*100, 'th percentile'))))
      } else {
        x <- x %>% 
          dplyr::ungroup() %>%
          tidyr::gather(key = 'Observations', value = 'value', dplyr::one_of('obs50')) %>% 
          dplyr::mutate(Observations = factor(.$Observations, levels = 'obs50', labels = 'Median'))
      }
      
      if ('strat2' %in% colnames(x)) {
        x$strat1 <- stringr::str_replace(x$strat1, stringr::str_c(vpc_dat$stratify[1], '='), '')
        x$strat2 <- stringr::str_replace(x$strat2, stringr::str_c(vpc_dat$stratify[2], '='), '')
        colnames(x)[colnames(x) == 'strat1'] <- vpc_dat$stratify[1]
        colnames(x)[colnames(x) == 'strat2'] <- vpc_dat$stratify[2]
      } else if (!is.null(vpc_dat$stratify)) {
        x[, vpc_dat$stratify] <- stringr::str_replace(x$strat, stringr::str_c(vpc_dat$stratify, '='), '')
      }
      dplyr::mutate(.data = x, group = as.numeric(interaction(x$strat, x$Observations)))
    }) %>% 
    c(list(opt = opt, psn = ifelse(!is.null(psn_folder), TRUE, FALSE), psn_bins = psn_bins,
           vpc_dir = ifelse(!is.null(psn_folder), psn_folder, xpdb$options$dir), 
           facets = facets, obs_problem = obs_problem, sim_problem = sim_problem, 
           obs_cols = obs_cols, sim_cols = sim_cols, nsim = vpc_nsim)) %>%
    {dplyr::tibble(problem = vpc_prob, method = 'vpc', type = vpc_type, data = list(.), modified = FALSE)} %>%
    {dplyr::bind_rows(xpdb$special, .)} %>% 
    dplyr::distinct(!!!rlang::syms(c('problem', 'method', 'type')), .keep_all = TRUE)
  
  msg('\nVPC done', quiet)
  as.xpdb(xpdb)
}
