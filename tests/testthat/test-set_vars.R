context('Check set_vars')

# Tests start here --------------------------------------------------------
test_that("input is check properly", {
  expect_error(set_var_types())
  expect_error(set_var_units())
  expect_error(set_var_types(xpdb_ex_pk, problem = 99), regexp = 'not found in model')
  expect_error(set_var_labels(xpdb_ex_pk, problem = 99), regexp = 'not found in model')
})

test_that("set_vars_type works properly", {
  xpdb_2 <- set_var_types(xpdb_ex_pk, problem = NULL, 
                          idv = 'PRED', catcov = c('SS', 'II'), fake = 'HELLO')
  expect_true(is.xpdb(xpdb_2))
  expect_identical(set_var_types(xpdb_ex_pk, problem = NULL), xpdb_ex_pk)
  expect_equal(xpdb_2$data$index[[1]]$col[xpdb_2$data$index[[1]]$type == 'idv'], 'PRED')
  expect_equal(xpdb_2$data$index[[1]]$type[xpdb_2$data$index[[1]]$col == 'TAD'], 'na')
  expect_equal(xpdb_2$data$index[[1]]$type[xpdb_2$data$index[[1]]$col == 'SS'], 'catcov')
  expect_equal(class(xpdb_2$data$data[[1]]$SS), 'factor')
})

test_that("set_vars_units and set_vars_label works properly", {
  xpdb_3 <- set_var_units(xpdb_ex_pk, problem = NULL, 
                          CL = 'L/h', V = 'L', fake = 'HELLO')
  expect_true(is.xpdb(xpdb_3))
  expect_identical(set_var_labels(xpdb_ex_pk, problem = NULL), xpdb_ex_pk)
  expect_equal(xpdb_3$data$index[[1]]$units[xpdb_3$data$index[[1]]$col == 'CL'], 'L/h')
  expect_equal(xpdb_3$data$index[[1]]$units[xpdb_3$data$index[[1]]$col == 'V'], 'L')
})
