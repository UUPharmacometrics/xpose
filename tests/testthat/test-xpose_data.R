context('Check xpose_data')


# Load helper files -------------------------------------------------------
load(file = file.path('data', 'theme_readable_221.Rdata'))

# Tests start here --------------------------------------------------------

test_that('error is returned when missing file and runno arguments', {
  expect_error(xpose_data(), regexp = 'Argument `runno` or `file` required')
})

test_that('error is returned when file does not exist', {
  expect_error(xpose_data(file = 'fake_mod.lst', dir = 'data'), 
               regexp = 'Model file fake_mod.lst not found')
})

test_that('error is returned for bad ext input', {
  expect_error(xpose_data(runno = '001', ext = 'pdf', dir = 'data'),
               regexp = 'Model file currently not supported by xpose')
})

test_that('error is returned for bad themes input', {
  expect_error(xpose_data(runno = '001', dir = 'data', xp_theme = list()),
               regexp = 'Argument `xp_theme` must be a full xpose theme')
  expect_error(xpose_data(runno = '001', dir = 'data', gg_theme = list()),
               regexp = 'Argument `gg_theme` must be a full ggplot2 theme')
  expect_error(xpose_data(runno = '001', dir = 'data', gg_theme = theme(legend.position = 'top')),
               regexp = 'Argument `gg_theme` must be a full ggplot2 theme')
})


test_that('properly creates the xpdb when using the file argument', {
  xpdb_1 <- xpose_data(file = 'run001.lst', dir = 'data', 
                       gg_theme = theme_readable_221, quiet = TRUE)
  expect_true(inherits(xpdb_1, 'xpose_data'))
  
  skip_on_cran() # Skip to avoid issue with no long double
  
  # Correct path and quiet option for identical comparison with xpdb_ex_pk
  xpdb_1$summary$value[xpdb_1$summary$label == 'dir'] <- 'analysis/models/pk/' 
  xpdb_1$options$dir <- 'analysis/models/pk/'
  attr(xpdb_1$code, 'dir') <- 'analysis/models/pk/'
  attr(xpdb_1$gg_theme, 'theme') <- 'theme_readable'
  xpdb_1$options$quiet <- FALSE
  xpdb_1$xp_theme$labeller <- xpdb_ex_pk$xp_theme$labeller
  expect_identical(xpdb_1, xpdb_ex_pk)
})

test_that('properly creates the xpdb when using the runno argument', {
  xpdb_2 <- xpose_data(runno = '001', ext = '.lst', dir = 'data', 
                       gg_theme = theme_readable_221, quiet = TRUE)
  expect_true(inherits(xpdb_2, 'xpose_data'))
  
  skip_on_cran() # Skip to avoid issue with no long double
  
  # Correct path and quiet option for identical comparison with xpdb_ex_pk
  xpdb_2$summary$value[xpdb_2$summary$label == 'dir'] <- 'analysis/models/pk/'
  xpdb_2$options$dir <- 'analysis/models/pk/'
  attr(xpdb_2$code, 'dir') <- 'analysis/models/pk/'
  attr(xpdb_2$gg_theme, 'theme') <- 'theme_readable'
  xpdb_2$options$quiet <- FALSE
  xpdb_2$xp_theme$labeller <- xpdb_ex_pk$xp_theme$labeller
  expect_identical(xpdb_2, xpdb_ex_pk)
})

test_that('properly handles errors in tables', {
  expect_warning(xpdb_3 <- xpose_data(runno = '001', ext = '.lst', dir = 'data', 
                                      ignore = c('files', 'summary'), quiet = TRUE, 
                                      manual_import = manual_nm_import(tab_names = 'badtab')),
                 regexp = 'Dropped `badtab001`')
  expect_error(dv_vs_pred(xpdb_3), regex = 'No `data` slot could be found in this xpdb')
})  

test_that('properly handles errors in summary', {
  broken_theme <- theme_xp_default()
  broken_theme$rounding <- 'No'
  expect_warning(xpdb_4 <- xpose_data(runno = '001', ext = '.lst', dir = 'data', 
                                      ignore = c('data', 'files'),
                                      quiet = TRUE, xp_theme = broken_theme),
                 regexp = 'Failed to create run summary')
  expect_error(summary(xpdb_4), regex = 'No `summary` slot could be found in this xpdb')
})  

test_that('properly handles errors in files', {
  expect_warning(xpdb_5 <- xpose_data(runno = '001', ext = '.lst', dir = 'data', 
                                      ignore = c('data', 'summary'), quiet = FALSE,
                                      extra_files = c('.lst', '.mod')),
                 regexp = 'Dropped `run001.lst`')
  expect_error(grd_vs_iteration(xpdb_5), regex = 'No `files` slot could be found in this xpdb')
})  

test_that('properly uses mapped ID column', {
  xpdb_1 <- xpose_data(file = 'run004.lst', dir = 'data', quiet = TRUE)
})
  