context('Check xpdb_edits')

# Tests start here --------------------------------------------------------

test_that("filter checks error properly", {
  expect_error(filter.xpose_data(.data = '1', ID == 110), regexp = 'Bad input')
  expect_error(filter.xpose_data(.data = xpdb_ex_pk, ID == 110, .problem = 99), 
               regexp = 'not found')
  expect_error(filter.xpose_data(.data = xpdb_ex_pk, .source = 'fake', ID == 110), regexp = 'not found')
  expect_error(filter.xpose_data(.data = xpdb_ex_pk, .source = c('ext', 'phi'), ID == 110), regexp = 'length 1')
})

test_that("filter works error properly", {
  expect_equal(filter.xpose_data(.data = xpdb_ex_pk, ID == 110, TIME > 10, .problem = 1) %>% get_data(problem = 1), 
               xpdb_ex_pk %>% get_data(problem = 1) %>% filter(.$ID == 110, .$TIME > 10))
  expect_equal(filter.xpose_data(.data = xpdb_ex_pk, ID == 110, .problem = 1, .source = 'phi') %>% 
                 get_file(ext = 'phi'), 
               xpdb_ex_pk %>% get_file(ext = 'phi') %>% filter(.$ID == 110))
})

test_that("variable added with mutate is available in all problems", {
  xpdb_mod <- mutate.xpose_data(xpdb_ex_pk, VAR=1)
  expect_true(exists("VAR", xpdb_mod$data$data[[1]]))
  expect_true(exists("VAR", xpdb_mod$data$data[[2]]))
})
