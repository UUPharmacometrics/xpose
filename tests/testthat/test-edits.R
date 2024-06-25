context('Check xpdb_edits')

# Define files to be tested -----------------------------------------------
ctrl_tab_1 <- xpdb_ex_pk %>% 
  mutate.xpose_data(DV = log(DV), .problem = 1) %>% 
  distinct.xpose_data(DV, .problem = 1) %>% 
  select.xpose_data(ID:TAD, DV, EVID, .problem = 1) %>% 
  rename.xpose_data(TSLD = TAD, .problem = 1) %>% 
  get_data(.problem = 1)

ctrl_tab_2 <- xpdb_ex_pk %>% 
  group_by(ID, SEX, .problem = 1) %>% 
  summarize(CMAX = max(DV), .problem = 1) %>% 
  ungroup(.problem = 1) %>% 
  get_data(.problem = 1) %>% 
  ungroup()

test_xpdb_1 <- vpc_data(xpdb_ex_pk, opt = vpc_opt(n_bins = 2), quiet = TRUE)
ctrl_xpdb_1 <- test_xpdb_1
ctrl_xpdb_1$special$data[[1]]$vpc_dat <- dplyr::filter(.data = ctrl_xpdb_1$special$data[[1]]$vpc_dat, bin == 2)

ctrl_list_vars_1 <- '\nList of available variables for problem no. 1 \n - Subject identifier (id)               : ID\n - Dependent variable (dv)               : DV\n - Independent variable (idv)            : TIME\n - Dose amount (amt)                     : AMT\n - Event identifier (evid)               : EVID\n - Model typical predictions (pred)      : PRED\n - Model individual predictions (ipred)  : IPRED\n - Model parameter (param)               : KA, CL, V, ALAG1\n - Eta (eta)                             : ETA1, ETA2, ETA3\n - Residuals (res)                       : CWRES, IWRES, RES, WRES\n - Categorical covariates (catcov)       : SEX, MED1, MED2\n - Continuous covariates (contcov)       : CLCR, AGE, WT\n - Compartment amounts (a)               : A1, A2\n - Not attributed (na)                   : DOSE, SS, II, CPRED, DV2\n\nList of available variables for problem no. 2 \n - Subject identifier (id)               : ID\n - Dependent variable (dv)               : DV\n - Independent variable (idv)            : TIME\n - Dose amount (amt)                     : AMT\n - Event identifier (evid)               : EVID\n - Model individual predictions (ipred)  : IPRED\n - Not attributed (na)                   : DOSE, SEX, CLCR, AGE, WT, DV2'

# Tests start here --------------------------------------------------------

test_that('xpdb_edits checks error properly', {
  expect_error(filter.xpose_data(.data = '1', ID == 110), regexp = 'Bad input')
  expect_error(filter.xpose_data(.data = xpdb_ex_pk, ID == 110, .problem = 99), 
               regexp = 'not found')
  expect_error(filter.xpose_data(.data = xpdb_ex_pk, .source = 'fake', ID == 110), regexp = 'not found')
  expect_error(filter.xpose_data(.data = xpdb_ex_pk, .source = c('ext', 'phi'), ID == 110), regexp = 'length 1')
  expect_error(filter.xpose_data(.data = xpdb_ex_pk, .source = 'special'))
})

test_that('xpdb_edits works properly', {
  expect_equal(filter.xpose_data(.data = xpdb_ex_pk, ID == 110, TIME > 10, .problem = 1) %>% 
                 get_data(.problem = 1), 
               xpdb_ex_pk %>% get_data(.problem = 1) %>% filter(.$ID == 110, .$TIME > 10))
  expect_equal(filter.xpose_data(.data = xpdb_ex_pk, ID == 110, .problem = 1, .source = 'phi') %>% 
                 slice.xpose_data(1:3, .source = 'phi') %>% 
                 mutate.xpose_data(TEST = 'Ok', .source = 'phi') %>% 
                 get_file(ext = 'phi', quiet = TRUE), 
               xpdb_ex_pk %>% get_file(ext = 'phi', quiet = TRUE) %>% filter(.$ID == 110) %>% 
                 slice(1:3) %>% mutate(TEST = 'Ok'))
  expect_equal(ctrl_tab_1, xpdb_ex_pk %>% 
                 get_data(.problem = 1) %>% 
                 mutate(DV = log(DV)) %>% 
                 distinct(DV, .keep_all = TRUE) %>% 
                 select(ID:TAD, DV, EVID) %>% 
                 rename(TSLD = TAD))
  expect_equal(ctrl_tab_2, xpdb_ex_pk %>% 
                 get_data(.problem = 1) %>% 
                 group_by(ID, SEX) %>% 
                 summarize(CMAX = max(DV)) %>% 
                 ungroup())
  expect_warning(test_xpdb_2 <- filter.xpose_data(.data = test_xpdb_1, bin == 2, .source = 'special', 
                                                  .where = c('fake1', 'fake2', 'vpc_dat'))$special$data[[1]],
                 regexp = 'elements fake1, fake2 not found in vpc continuous')
  expect_equal(test_xpdb_2, ctrl_xpdb_1$special$data[[1]])
})

test_that('variable added with mutate is available in all problems', {
  xpdb_mod <- mutate.xpose_data(xpdb_ex_pk, VAR = 1)
  expect_true(exists('VAR', xpdb_mod$data$data[[1]]))
  expect_true(exists('VAR', xpdb_mod$data$data[[2]]))
})

test_that('index is properly updated', {
  xpdb_mod <- mutate.xpose_data(.data = xpdb_ex_pk, TAD = NULL, DV2 = DV * 2)
  expect_equal(capture_output(list_vars(xpdb_mod, .problem = 1:2)), ctrl_list_vars_1)
})

test_that('irep works properly', {
  expect_message(irep_out <- irep(rep(1:5, time = 3), quiet = FALSE),
                 regexp = '3 simulations found')
  expect_equal(irep_out, rep(1:3, each = 5))
  expect_message(irep_out2 <- irep(rep(c(10,5,6), time = 7), quiet = FALSE),
                 regexp = '7 simulations found')
  expect_equal(irep_out2, rep(1:7, each = 3))
})

test_that('n() works', {
  xpdb_sum <- xpdb_ex_pk %>% group_by(ID, .problem = 1) %>% summarise(n(), .problem = 1)
  expect_equal(xpdb_sum %>% get_data(.problem = 1), 
               xpdb_ex_pk %>% get_data(.problem = 1) %>% group_by(ID) %>% summarise(n()))
  expect_equal(xpdb_ex_pk %>% mutate(N = 1:n(), .source = 'ext') %>% get_file(ext = 'ext', quiet = TRUE),
               xpdb_ex_pk %>% get_file(ext = 'ext', quiet = TRUE) %>% mutate(N = 1:n()))
})

