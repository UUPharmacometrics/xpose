xpdb_NULL       <- xpdb_ex_pk
xpdb_NULL$data  <- NULL
xpdb_NULL$files <- NULL
xpdb_NULL       <- as.xpdb(xpdb_NULL)
mapping <- aes(x = TIME)

# Tests start here --------------------------------------------------------
# test_that('Check check_vars', {
#   expect_null(check_vars(NULL, xpdb_ex_pk))
#   expect_null(check_vars(c('problem', 'simtab', 'index', 'data'), xpdb_ex_pk))
# })

test_that('Check check_scales', {
  expect_equal(check_scales('x', NULL), 'continuous')
  expect_equal(check_scales('x', 'y'), 'continuous')
  expect_equal(check_scales('x', 'xy'), 'log10')
})

test_that('Check parse_title', {
  expect_equal(parse_title('OFV: @ofv', xpdb_ex_pk, problem = 1, quiet = TRUE), 'OFV: -1403.905')
  expect_warning(tmp_title1 <- parse_title('OFV: @fake', xpdb_ex_pk, problem = 1, quiet = FALSE), 
                 regexp = 'not part of')
  expect_equal(tmp_title1, 'OFV: @fake')
  expect_equal(parse_title('OFV: @fake', xpdb_ex_pk, problem = 1, quiet = TRUE, extra_key = 'fake', 
                           extra_value = '1987'), 'OFV: 1987')
  expect_warning(tmp_title2 <- parse_title('OFV: @ofv, @ignored', xpdb_ex_pk, problem = 1, quiet = TRUE, 
                                           ignore_key = 'ignored'), NA)
  expect_equal(tmp_title2, 'OFV: -1403.905, @ignored')
})

test_that('Check filter_xp_theme', {
  # Keep matches
  expect_equal(filter_xp_theme(xpdb_ex_pk$xp_theme, 'point_', action = 'keep'),
               xpdb_ex_pk$xp_theme[grepl('point_', names(xpdb_ex_pk$xp_theme))])
  # Drop matches
  expect_equal(filter_xp_theme(xpdb_ex_pk$xp_theme, 'point_', action = 'drop'),
               xpdb_ex_pk$xp_theme[!grepl('point_', names(xpdb_ex_pk$xp_theme))])
})

test_that('Check all_data_problem', {
  expect_equal(all_data_problem(xpdb_NULL), NA_integer_)
  expect_equal(all_data_problem(xpdb_ex_pk), 1:2)
})

test_that('Check last_data_problem', {
  expect_error(last_data_problem(xpdb_NULL))
  expect_equal(last_data_problem(xpdb_ex_pk, simtab = FALSE), 1)
  expect_equal(last_data_problem(xpdb_ex_pk, simtab = TRUE), 2)
})

test_that('Check all_file_problem', {
  expect_error(all_file_problem(xpdb_NULL))
  expect_equal(all_file_problem(xpdb_ex_pk, ext = 'ext'), 1)
  expect_equal(all_file_problem(xpdb_ex_pk, ext = 'fake'), NA_integer_)
})

test_that('Check last_file_problem', {
  expect_equal(last_file_problem(xpdb_ex_pk, ext = 'ext'), 1)
})

test_that('Check last_file_subprob', {
  expect_equal(last_file_subprob(xpdb_ex_pk, ext = 'ext', .problem = 1), 1)
  expect_equal(last_file_subprob(xpdb_ex_pk, ext = 'fake', .problem = 1), NA_integer_)
})

test_that('Check xp_var', {
  expect_equal(xp_var(xpdb_ex_pk, 1, col = 'TIME')$type, 'idv')
  expect_equal(xp_var(xpdb_ex_pk, 1, type = 'idv')$col, 'TIME')
  expect_null(xp_var(xpdb_ex_pk, 1, col = 'FAKE_COL', silent = TRUE))
  expect_error(xp_var(xpdb_ex_pk, 1, col = 'FAKE_COL', silent = FALSE),
               regexp = 'Column `FAKE_COL` not available')
})

test_that('Check append_aes', {
  expect_equal(aes_c(aes(x = .data[["IPRED"]], y = .data[["DV"]]), NULL), 
               aes(x = .data[["IPRED"]], y = .data[["DV"]]))
  expect_equal(aes_c(aes(x = .data[["IPRED"]], y = .data[["DV"]]), 
                     aes(y = .data[["PRED"]])), 
               aes(x = .data[["IPRED"]], y = .data[["PRED"]]))
})

test_that('Check check_xpdb', {
  expect_error(check_xpdb(xpdb = '1', check = 'data'), regexp = 'Bad input')
  expect_error(check_xpdb(xpdb_NULL, check = 'data'), regexp = 'No `data` slot could be found in this xpdb')
  expect_null(check_xpdb(xpdb_NULL, check = FALSE))
  expect_null(check_xpdb(xpdb_ex_pk, check = 'data'))
})

test_that('Check check_plot_type', {
  expect_silent(check_plot_type('pls', allowed = c('p', 'l', 's', 't')))
  expect_warning(check_plot_type('prlst', allowed = c('p', 'l', 's', 't')), 
                 regexp = 'Plot type \"r\" not recognized')
  expect_warning(check_plot_type('prsz', allowed = c('p', 'l', 's', 't')), 
                 regexp = 'Plot type \"r\", \"z\" not recognized')
})

test_that('Check drop_fixed', {
  expect_message(variable_cols <- drop_fixed_cols(xpdb_ex_pk, .problem = 1, quiet = FALSE,
                                                  cols = c('CL', 'V', 'KA', 'ALAG1')), 
                 regexp = 'Dropped fixed variables ALAG1')
  expect_equal(variable_cols, c('CL', 'V', 'KA'))
})

test_that('Check add_facets_var', {
  expect_equal(add_facet_var(facets = c('OCC', 'SEX'), variable = 'variable'),
               c('variable', 'OCC', 'SEX'))
  expect_equal(add_facet_var(facets = as.formula('OCC~SEX'), variable = 'variable'),
               as.formula('OCC~SEX+variable'))
})

test_that('Check get_aes', {
  expect_error(get_aes(mapping))
  expect_equal(get_aes(mapping, x), 'TIME')
})

