#' Visual predictive checks data
#'
#' @description Generate visual predictive checks (VPC) data
#' 
#' @param xpdb An xpose database object.
#' @param opt A list of options regarding binning, pi and ci computation. 
#' For more information see \code{\link{vpc_opt}}.
#' @param vpc_type A string specifying the type of VPC to be created, can be one of: 
#' 'continuous', 'categorical', 'censored' or 'time-to-event'.
#' @param stratify Either a character string or a formula to stratify the data.
#' @param psn_folder Specify a PsN-generated VPC-folder.
#' @param obs_problem Alternative to the option `psn_folder`. The $problem number to 
#' be used for observations. By default returns the last estimation problem.
#' @param sim_problem Alternative to the option `psn_folder`. The $problem number to 
#' be used for simulations. By default returns the last simulation problem.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... any additional aesthetics.
#' 
#' @seealso \code{vpc} \code{vpc_opt} \code{\link[vpc]{vpc}}
#' @examples
#' \dontrun{
#' xpdb_ex_pk %>% 
#'  vpc_data() %>% 
#'  vpc()
#' }
#' @export
vpc_data <- function(xpdb,
                     opt         = NULL,
                     vpc_type    = c('continuous', 'categorical', 'censored', 'time-to-event'),
                     stratify    = NULL,
                     psn_folder  = NULL,
                     obs_problem = NULL,
                     sim_problem = NULL,
                     quiet) {
  # Check input
  check_xpdb(xpdb, check = ifelse(is.null(psn_folder), 'data', FALSE))
  if (missing(quiet)) quiet <- xpdb$options$quiet
  
  # Set vpc_data options
  if (is.null(opt)) opt <- vpc_opt()
  
  # Get raw data
  if (is.null(psn_folder)) {
    # When using xpdb tables
    if (is.null(obs_problem)) obs_problem <- last_data_problem(xpdb, simtab = FALSE)
    if (is.null(sim_problem)) sim_problem <- last_data_problem(xpdb, simtab = TRUE)
    msg(c('Using xpdb simulation problem ', ifelse(is.na(sim_problem), '<na>', sim_problem), 
          ' and observation problem ', ifelse(is.na(obs_problem), '<na>', obs_problem), '.'), quiet)
    obs_data <- get_data(xpdb, problem = obs_problem) 
    sim_data <- get_data(xpdb, problem = sim_problem)
    if (is.null(opt$obs_cols)) {
      obs_cols <- xp_var(xpdb, obs_problem, type = c('id', 'idv', 'dv', 'pred'))
      obs_cols <- purrr::set_names(obs_cols$col, nm = obs_cols$type)
    }
    if (is.null(opt$sim_cols)) {
      sim_cols <- xp_var(xpdb, sim_problem, type = c('id', 'idv', 'dv', 'pred'))
      sim_cols <- purrr::set_names(sim_cols$col, nm = sim_cols$type)
    }
  } else {
    # When using PsN
    if (!dir.exists(psn_folder)) {
      stop('The `psn_folder`:', psn_folder, ' could not be found.', call. = FALSE)
    }
    msg('Importing PsN generated data', quiet)
    
    if (!dir.exists(file_path(psn_folder, 'm1')) &
        file.exists(file_path(psn_folder, 'm1.zip'))) {
      msg('Unziping PsN m1 folder.', quiet)
      utils::unzip(zipfile = file_path(psn_folder, 'm1.zip'), 
                   exdir   = file_path(psn_folder, ''))
      unzip <- TRUE
    } else {
      unzip <- FALSE 
    }
    obs_data <- read_nm_tables(files = file.path(psn_folder, 'm1', dir(stringr::str_c(psn_folder, 'm1', sep = .Platform$file.sep), 
                                                                       pattern = 'original.npctab')[1]), quiet = TRUE)
    sim_data <- read_nm_tables(files = file.path(psn_folder, 'm1', dir(stringr::str_c(psn_folder, 'm1', sep = .Platform$file.sep), 
                                                                       pattern = 'simulation.1.npctab')[1]), quiet = TRUE)
    if (unzip) unlink(x = file_path(psn_folder, 'm1'), recursive = TRUE, force = TRUE)
    
    # Getting multiple options form the psn command
    if (file.exists(file_path(psn_folder, 'command.txt'))) {
      psn_cmd  <- readr::read_lines(file = file.path(psn_folder, 'command.txt'))
      obs_cols <- get_psn_vpc_cols(psn_cmd)
      sim_cols <- obs_cols
      if (is.null(stratify)) stratify <- get_psn_vpc_strat(psn_cmd)
      if (is.null(opt$pred_corr)) {
        opt$pred_corr <- dplyr::if_else(stringr::str_detect(psn_cmd, '-predcorr'), TRUE, FALSE)
      }
      if (is.null(opt$lloq)) {
        lloq <- as.numeric(stringr::str_match(psn_cmd, '-lloq=([^\\s]+)')[1, 2])
        if (!is.na(lloq)) opt$lloq <- lloq
      }
      if (is.null(opt$uloq)) {
        uloq <- as.numeric(stringr::str_match(psn_cmd, '-uloq=([^\\s]+)')[1, 2])
        if (!is.na(uloq)) opt$uloq <- uloq
      }
    } else {
      msg('File `command.txt` not found. Using default column names: ID, TIME, DV, PRED.', quiet)
      obs_cols <- c(id = 'ID', idv = 'TIME', dv = 'DV', pred = 'PRED')
      sim_cols <- obs_cols
    }
  } 
  
  if (is.null(obs_data) && is.null(sim_data)) {
    msg('No data table found.', quiet)
    return()
  }
  
  if (is.null(opt$pred_corr)) opt$pred_corr <- FALSE
  
  # Get the type of vpc
  vpc_type <- match.arg(vpc_type)
  
  # Info on stratification
  facets <- stratify
  if (is.formula(stratify)) stratify <- all.vars(stratify)
  if (!is.null(stratify)) {
    msg(c('Using ', stringr::str_c(stratify, collapse = ', '), 
          ' for stratification.'), quiet)
  }
  if (!is.null(opt$lloq)) msg(c('Setting lloq to ', opt$lloq, '.'), quiet)
  if (!is.null(opt$uloq)) msg(c('Setting uloq to ', opt$uloq, '.'), quiet)
  
  # Generate vpc data
  if (vpc_type == 'continuous') {
    vpc_dat <- vpc::vpc(obs = obs_data, sim = sim_data, psn_folder = NULL, bins = opt$bins, 
                        n_bins = opt$n_bins, bin_mid = opt$bin_mid, obs_cols = obs_cols, 
                        sim_cols = sim_cols, stratify = stratify, pred_corr = opt$pred_corr, 
                        pred_corr_lower_bnd = opt$pred_corr_lower_bnd, pi = opt$pi, ci = opt$ci, 
                        uloq = opt$uloq, lloq = opt$lloq, smooth = FALSE, vpcdb = TRUE, verbose = !quiet) 
  } else if (vpc_type == 'categorical') {
    vpc_dat <- vpc::vpc_cat(obs = obs_data, sim = sim_data, psn_folder = NULL, bins = opt$bins, 
                            n_bins = opt$n_bins, bin_mid = opt$bin_mid, obs_cols = obs_cols, 
                            sim_cols = sim_cols, stratify = stratify, ci = opt$ci, 
                            uloq = opt$uloq, lloq = opt$lloq, smooth = FALSE, vpcdb = TRUE, verbose = !quiet) 
  } else if (vpc_type == 'censored') {
    vpc_dat <- vpc::vpc_cens(obs = obs_data, sim = sim_data, psn_folder = NULL, bins = opt$bins, 
                             n_bins = opt$n_bins, bin_mid = opt$bin_mid, obs_cols = obs_cols, 
                             sim_cols = sim_cols, stratify = stratify, ci = opt$ci, 
                             uloq = opt$uloq, lloq = opt$lloq, smooth = FALSE, vpcdb = TRUE, verbose = !quiet) 
  } else {
    msg('Time-to-event VPC are not yet available in xpose.', TRUE)
    return()
    # vpc_dat <- vpc::vpc_tte(obs = obs_data, sim = sim_data, psn_folder = NULL, bins = opt$bins, 
    #                         n_bins = opt$n_bins, obs_cols = obs_cols, sim_cols = sim_cols, stratify = stratify, 
    #                         ci = opt$ci, smooth = FALSE, rtte = opt$rtte, rtte_calc_diff = opt$rtte_calc_diff, 
    #                         events = opt$events, kmmc = opt$kmmc, reverse_prob = opt$reverse_prob, 
    #                         as_percentage = opt$as_percentage, vpcdb = TRUE, verbose = !quiet) 
  } 
  
  # Format vpc output
  xpdb$special <- vpc_dat %>%
    purrr::discard(names(.) %in% c('sim', 'stratify_original', 'stratify_color', 'facet', 'as_percentage')) %>%
    purrr::map_at('vpc_dat', function(x) {
      x <- x %>% 
        tidyr::gather(key = 'tmp', value = 'value', dplyr::matches('\\.(low|med|up)')) %>% 
        tidyr::separate_('tmp', into = c('Simulations', 'ci'), sep = '\\.') %>% 
        tidyr::spread_(key_col = 'ci', value_col = 'value')
      
      if (vpc_type == 'continuous') {
        x <- dplyr::mutate(.data = x, 
                           Simulations = factor(x$Simulations, levels = c('q5', 'q50', 'q95'),
                                                labels = c(stringr::str_c(min(opt$pi)*100, 'th percentile'), 
                                                           'Median', stringr::str_c(max(opt$pi)*100, 'th percentile'))))
      } else {
        x <- dplyr::mutate(.data = x, Simulations = factor(x$Simulations, levels = 'q50', labels = 'Median'))
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
          tidyr::gather(key = 'Observations', value = 'value', dplyr::one_of('obs5', 'obs50', 'obs95')) %>% 
          dplyr::mutate(Observations = factor(.$Observations, levels = c('obs5', 'obs50', 'obs95'),
                                              labels = c(stringr::str_c(min(opt$pi)*100, 'th percentile'), 
                                                         'Median', stringr::str_c(max(opt$pi)*100, 'th percentile'))))
      } else {
        x <- x %>% 
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
    c(.,list(opt = opt, facets = facets, psn_folder = psn_folder,
             obs_problem = obs_problem, sim_problem = sim_problem, 
             obs_cols = obs_cols, sim_cols = sim_cols)) %>%
             {dplyr::tibble(problem = 0, method = 'vpc', type = vpc_type, data = list(.))} %>%
    dplyr::bind_rows(xpdb$special) %>%
    dplyr::distinct_(.dots = c('problem', 'method', 'type'), .keep_all = TRUE)
  xpdb
}
