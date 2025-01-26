xp1 <- set_var_types(xpdb_ex_pk, catcov = 'DOSE')
xp1 <- set_var_labels(xp1, .problem = 1, ALAG1 = 'Lag time', CL = 'Clearance')
xp1 <- set_var_units(xp1, .problem = 1, ALAG1 = 'h', CL = 'L/h')
xp2 <- set_var_labels_units(
  xpdb_ex_pk,
  info = data.frame(
    col = c('ALAG1', 'CL', 'V'),
    label = c('Lag time', 'Clearance', 'Volume'),
    units = c('h', NA, 'L')
  )
)
xp2 <- set_var_labels(xp2, .problem = 1, V = 'Central volume')

# Tests start here --------------------------------------------------------
test_that('input is check properly', {
  expect_error(get_var_types(), regexp = 'argument \"xpdb\" is missing')
  expect_error(get_var_labels(), regexp = 'argument \"xpdb\" is missing')
  expect_error(get_var_units(), regexp = 'argument \"xpdb\" is missing')
  expect_error(get_var_labels_units(), regexp = 'argument \"xpdb\" is missing')
  expect_error(get_var_types(xpdb_ex_pk, .problem = 99), regexp = 'not found in model')
  expect_error(get_var_labels(xpdb_ex_pk, .problem = 99), regexp = 'not found in model')
  expect_error(get_var_units(xpdb_ex_pk, .problem = 99), regexp = 'not found in model')
  expect_error(get_var_labels_units(xpdb_ex_pk, .problem = 99), regexp = 'not found in model')
  expect_error(get_var_types(xpdb_ex_pk, .problem = 1, zzz), regexp = 'object \'zzz\' not found')
  expect_error(get_var_labels(xpdb_ex_pk, .problem = 1, zzz), regexp = 'object \'zzz\' not found')
  expect_error(get_var_units(xpdb_ex_pk, .problem = 1, zzz), regexp = 'object \'zzz\' not found')
  expect_error(get_var_labels_units(xpdb_ex_pk, .problem = 99, zzz), regexp = 'object \'zzz\' not found')
  
})

test_that('get_vars_type works properly', {
  expect_true(is.null(get_var_types(xpdb_ex_pk, 'HELLO')))
  expect_type(get_var_types(xpdb_ex_pk, 'ID'), 'character')
  expect_equal(
    unname(get_var_types(xp1, 'TIME', 'PRED', 'DOSE')),
    c('idv', 'pred', 'catcov')
  )
})

test_that('get_var_labels works properly', {
  expect_true(is.null(get_var_labels(xp1, .problem = 1, 'zzz')))
  expect_true(get_var_labels(xp1, .problem = 1, 'TIME') == 'TIME')
  expect_true(all(get_var_labels(xp1, .problem = 1, 'ALAG1', 'CL') %in% c('Lag time', 'Clearance')))
})

test_that('get_var_units works properly', {
  expect_true(is.null(get_var_units(xp1, .problem = 1, 'zzz')))
  expect_true(is.na(get_var_units(xp1, .problem = 1, 'TIME')))
  expect_true(all(get_var_units(xp1, .problem = 1, 'ALAG1', 'CL') %in% c('h', 'L/h')))
})

test_that('get_var_labels_units works properly', {
  expect_true(is.null(get_var_labels_units(xp2, 'zzz', .problem = 1)))
  expect_true(is.null(get_var_labels_units(xp2, 'zzz')))
  expect_true(get_var_labels_units(xp1, 'TIME', .problem = 1) == 'TIME')
  expect_true(get_var_labels_units(xp1, 'TIME') == 'TIME')
  expect_true(get_var_labels_units(xp2, 'CL') == 'Clearance')
  expect_true(
    all(get_var_labels_units(xp2, 'ALAG1', 'V') == c('Lag time (h)', 'Volume (L)'))
  )
  expect_true(
    all(get_var_labels_units(xp2, 'ALAG1', 'V', .problem = 1) == c('Lag time (h)', 'Central volume (L)'))
  )
})
