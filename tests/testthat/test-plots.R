prediction_plot_functions <- c('dv_vs_pred', 'dv_vs_ipred', 'dv_vs_idv', 
                               'ipred_vs_idv', 'pred_vs_idv', 'dv_preds_vs_idv')
residual_plot_functions   <- c('res_vs_idv', 'res_vs_pred', 'absval_res_vs_idv', 
                               'absval_res_vs_pred', 'res_distrib', 'res_qq') 
iteration_plot_functions  <- c('prm_vs_iteration', 'grd_vs_iteration')
parameter_plot_functions  <- c('prm_distrib', 'prm_qq')
eta_plot_functions        <- c('eta_distrib', 'eta_qq')
cov_plot_functions        <- c('cov_distrib', 'cov_qq')
kinetic_plot_functions    <- c('amt_vs_idv')
individual_plot_functions <- c('ind_plots')

plot_functions <- c(prediction_plot_functions, residual_plot_functions, 
                    iteration_plot_functions, parameter_plot_functions, 
                    eta_plot_functions, cov_plot_functions, 
                    individual_plot_functions, kinetic_plot_functions)

not_sim_functions      <- plot_functions[!grepl('iteration|prm_|eta_|cov_|res_qq|res_distrib|amt_|pred|ind_plots', plot_functions)]
distribution_functions <- plot_functions[grepl('(prm|eta|cov|res)_(distrib|qq)', plot_functions)]

# Simulation only xpdb
xpdb_sim_only <- xpose_data(file = 'sim.lst', dir = 'data', quiet = TRUE)

# Missing all output files except grd
xpdb_mis_file <- xpose_data(file = 'sim.lst', dir = 'data', 
                            ignore = c('data', 'summary'), quiet = TRUE,
                            extra_files = '.phi')
# No output files at all
xpdb_no_file <- xpose_data(file = 'sim.lst', dir = 'data', quiet = TRUE,
                           ignore = c('data', 'files', 'summary'))

# Tests start here --------------------------------------------------------
test_that(
  'errors are returned when xpdb_ex_pk is missing', {
    purrr::walk(
      plot_functions,
      \(pl_fn) { 
        args <- list()
        expect_error(rlang::exec(.f = pl_fn, !!!args))
      })
  })


test_that(
  'errors are returned when length .problem is > 1', {
    purrr::walk(
      plot_functions,
      \(pl_fn) { 
        args <- list(xpdb_ex_pk, .problem = 1:2)
        expect_error(rlang::exec(.f = pl_fn, !!!args), regexp = 'must be of length 1')
      })
  })


test_that(
  'errors are returned a .problem not in the data', {
    purrr::walk(
      plot_functions,
      \(pl_fn) { 
        args <- list(xpdb_ex_pk, .problem = 99)
        expect_error(rlang::exec(.f = pl_fn, !!!args), regexp = '99 not found in')
      })
  })


test_that(
  'xpose plot objects are returned with appropriate xpdb_ex_pk', {
    purrr::walk(
      plot_functions,
      \(pl_fn) { 
        args <- list(xpdb_ex_pk)
        expect_true(is.xpose.plot(rlang::exec(.f = pl_fn, !!!args)))
      })
  })


test_that(
  'xpose plot objects are returned with appropriate xpdb_ex_pk and muliple residuals', {
    purrr::walk(
      residual_plot_functions,
      \(pl_fn) { 
        args <- list(xpdb_ex_pk, res = c('CWRES', 'IWRES'))
        expect_true(is.xpose.plot(rlang::exec(.f = pl_fn, !!!args)))
      })
  })


test_that(
  'no error occurs when xpdb is from a simulation only', {
    purrr::walk(
      not_sim_functions,
      \(pl_fn) { 
        args <- list(xpdb_sim_only)
        expect_true(is.xpose.plot(rlang::exec(.f = pl_fn, !!!args)))
      })
  })


test_that(
  'have proper error check', {
    purrr::walk(
      iteration_plot_functions,
      \(pl_fn) { 
        args_1 <- list(xpdb_no_file)
        expect_error(
          rlang::exec(.f = pl_fn, !!!args_1), 
          regexp = 'No `files` slot could be found in this xpdb'
        )
        
        args_2 <- list(xpdb_mis_file)
        expect_error(
          rlang::exec(.f = pl_fn, !!!args_2), 
          regexp = 'File extension.+not found in model output files'
        )
      })
  })


test_that(
  'functions have proper error check', {
    ## Distribution functions
    purrr::walk(
      distribution_functions,
      \(pl_fn) { 
        args <- list(xpdb_sim_only)
        expect_error(
          rlang::exec(.f = pl_fn, !!!args), 
          regexp = 'Column.+ not available'
        )
      })
    
    ## Prm vs iteration 
    expect_error({
      xpdb_ex_pk %>% 
        filter(ITERATION == 0, .source = 'ext') %>% 
        prm_vs_iteration()
    },
    regexp = 'No parameters varying across ITERATION were found'
    )
    
    ## Eta QQ plots
    expect_error({
      xpdb_ex_pk %>% 
        mutate(ETA1 = 0, ETA2 = 0, ETA3 = 0) %>% 
        eta_qq(drop_fixed = TRUE)
    },
    regexp = 'No non-fixed variables available for plotting'
    )
    
    expect_true(
      is.xpose.plot(
        xpdb_ex_pk %>% 
          mutate(ETA1 = 0, ETA2 = 0, ETA3 = 0) %>% 
          eta_qq(drop_fixed = FALSE)
      )
    )
  })
