context('Check VPCs')

# Define files to be tested -----------------------------------------------

# ctrl_special <- xpdb_ex_pk %>%
#   vpc_data(opt = vpc_opt(n_bins = 3, lloq = 0.1)) %>%
#   vpc_data(vpc_type = 'cens', opt = vpc_opt(n_bins = 3, lloq = 0.4))
#  save(ctrl_special, file = 'data/ctrl_special.RData', compress = 'xz')
load(file = file.path('data', 'ctrl_special.RData'))

# ctrl_psn_vpc_dat <- psn_vpc_parser(xpdb = xpdb_ex_pk, psn_folder = 'data/psn_vpc/', 
#                                    psn_bins = TRUE, opt = vpc_opt(), quiet = TRUE)
# save(ctrl_psn_vpc_dat, file = 'data/ctrl_psn_vpc.RData', compress = 'xz')
load(file = file.path('data', 'ctrl_psn_vpc.RData'))


xpdb_vpc_test <- xpdb_ex_pk %>%
  vpc_data(opt = vpc_opt(n_bins = 3, lloq = 0.1), quiet = TRUE) %>%
  vpc_data(vpc_type = 'cens', opt = vpc_opt(n_bins = 3, lloq = 0.4), quiet = TRUE)

test_psn_vpc <- vpc_data(xpdb_ex_pk, psn_folder = 'data/psn_vpc/', quiet = TRUE)

# Tests start here --------------------------------------------------------
test_that('vpc_opt works properly', {
  expect_equal(vpc_opt(), 
               list(bins = 'jenks', n_bins = 'auto', bin_mid = 'mean', pred_corr = FALSE, 
                    pred_corr_lower_bnd = 0, pi = c(0.025, 0.975),  ci = c(0.025, 0.975),  
                    lloq = NULL,  uloq = NULL, rtte = FALSE, rtte_calc_diff = TRUE, 
                    events = NULL, kmmc = NULL, reverse_prob = FALSE, 
                    as_percentage = TRUE, usr_call = NULL))
})

test_that('vpc_data properly check input', {
  expect_error(vpc_data(), regexp = 'argument \"xpdb\" is missing')
  expect_error(vpc_data(xpdb_ex_pk, psn_folder = '.', quiet = TRUE), 
                 regexp = 'No table files could be found')
  expect_error(vpc_data(xpdb_ex_pk, psn_folder = 'fake', quiet = TRUE), 
               regexp = 'fake could not be found')
})

test_that('vpc_data works properly with xpdb tables', {
  expect_true(is.xpdb(xpdb_vpc_test))
  expect_identical(xpdb_vpc_test$special, ctrl_special$special)
})

test_that('vpc_data works properly with psn_folder', {
  skip_on_cran()
  # Check psn_vpc_parser
  parsed_psn_vpc <- psn_vpc_parser(xpdb = xpdb_ex_pk, psn_folder = 'data/psn_vpc/', 
                                   psn_bins = TRUE, opt = vpc_opt(), quiet = TRUE)
  expect_equal(parsed_psn_vpc, ctrl_psn_vpc_dat)
  expect_true(is.xpdb(test_psn_vpc))
  expect_error(psn_vpc_plot <- test_psn_vpc %>% vpc(), NA)
})


test_that('vpc plot properly check input', {
  expect_error(vpc())
  expect_error(vpc(xpdb = 1, quiet = FALSE), regexp = 'Bad input')
  expect_error(vpc(xpdb_ex_pk, quiet = FALSE), regexp = 'No `special` slot')
  expect_error(vpc(ctrl_special, quiet = FALSE), regexp = 'Several VPC data')
  expect_error(vpc(ctrl_special, vpc_type = 'unknown', quiet = FALSE), 
               regexp = 'should be one of')
  expect_error(test_psn_vpc %>% vpc(vpc_type = 'cens'),
               regexp = 'Change `vpc_type` to continuous')
})

test_that('vpc plot are properly generated', {
  p_cont  <- vpc(ctrl_special, vpc_type = 'continuous', type = 'alrpt', quiet = FALSE)
  p_cont2 <- vpc(ctrl_special, vpc_type = 'continuous', facets = ~group)
  p_cens  <- vpc(ctrl_special, vpc_type = 'censored', smooth = FALSE, facets = 'group', 
                 type = 'alr', quiet = FALSE)
  expect_true(is.xpose.plot(p_cont))
  expect_true(is.xpose.plot(p_cens))
  expect_equal(class(p_cont$layers[[1]]$geom)[1], 'GeomRibbon')
  expect_equal(class(p_cont$layers[[2]]$geom)[1], 'GeomLine')
  expect_equal(class(p_cont$layers[[3]]$geom)[1], 'GeomPoint')
  expect_equal(class(p_cont$layers[[4]]$geom)[1], 'GeomText')
  expect_equal(class(p_cont$layers[[5]]$geom)[1], 'GeomHline')
  expect_equal(class(p_cont$layers[[6]]$geom)[1], 'GeomRug')
  expect_equal(class(p_cont$facet)[1], 'FacetNull')
  expect_equal(class(p_cont2$facet)[1], 'FacetGrid')
  expect_equal(class(p_cens$layers[[1]]$geom)[1], 'GeomRect')
  expect_equal(class(p_cens$facet)[1], 'FacetWrap')
  expect_equal(p_cont$xpose$fun, 'vpc_continuous')
  expect_equal(p_cens$xpose$fun, 'vpc_censored')
  expect_equal(p_cont$xpose$problem, 3)
  expect_equal(p_cens$xpose$problem, 4)
  expect_equal(p_cont$xpose$summary$value[p_cont$xpose$summary$label %in% c('vpcdir', 'vpcnsim', 'vpcci', 'vpcpi')], 
              c('analysis/models/pk/', '20', '95', '95'))
})
