context('Check read_nm_tables')

# Define files to be tested -----------------------------------------------

test_tab <- read_nm_tables(file = 'sdtab001', dir = 'data', quiet = TRUE)
ctrl_tab <- get_data(xpdb_ex_pk, table = 'sdtab001')

test_file <- c('TABLE NO.  4',
               ' ID         ,KA         ,CL         ,V          ,ALAG1      ,ETA1       ,ETA2       ,ETA3',
               '  1.1000E+02, 4.9578E-01, 2.5517E+01, 1.4071E+02, 2.0796E-01,-3.7021E-02,-5.9613E-03,-2.1376E+00',
               '  1.1200E+02, 4.1061E+00, 2.1792E+01, 1.2183E+02, 2.0796E-01,-4.9451E-02, 1.2194E-01,-2.3484E-02')

ctrl_file <- get_data(xpdb_ex_pk, table = 'patab001') %>% 
  dplyr::distinct_(.dots = 'ID', .keep_all = TRUE) %>% 
  dplyr::slice(1:2) %>% 
  dplyr::mutate(ID = factor(.$ID, levels = c(110, 112)))

firstonly_test <- as.nm.table.list(dplyr::tibble(problem   = 1, 
                                                 file      = file_path('data', c('sdtab001', 'patab001')),
                                                 firstonly = c(TRUE, FALSE),
                                                 simtab    = FALSE))

minus_sign_test <- c('TABLE NO.  1',
                     ' ID          TIME        DV',
                     '  1.0000E+00  0.0000E+00  1.0000E+00',
                     '  1.0000E+00  1.0000E+00  1.0000E+00',
                     '  1.0000E+00  2.0000E+00 -5.0000E+00')

# Tests start here --------------------------------------------------------

test_that('error is returned when missing file argument', {
  expect_error(read_nm_tables(), regexp = 'Argument `file` required')
})

test_that('error is returned when all provided files are missing', {
  expect_error(read_nm_tables(file = 'fake_table.tab', quiet = TRUE), 
               regexp = 'No table files could be found')
})

test_that('error is returned when tables exist but are duplicated', {
  expect_error(read_nm_tables(file = c('sdtab001', 'patab001', 'sdtab001'), 
                              dir = 'data', quiet = TRUE),
               regexp = 'No table imported due to duplicated names')
})

test_that('error is returned when missing table header', {
  files <- tempfile('tmp_header')
  on.exit(unlink(files))
  
  # Test with skip = 1 and header = FALSE
  writeLines(text = test_file[c(1, 3:4)], con = files[1])
  expect_error(suppressWarnings(read_nm_tables(file = files[1], quiet = TRUE)), 
                 regexp = 'No table imported')
})

test_that('dot arguments are properly passed to readr', {
  expect_equal(nrow(read_nm_tables(file = 'sdtab001', dir = 'data', n_max = 3, quiet = TRUE)), 3)
})

test_that('returns a tibble when user mode is used', {
  skip_on_cran()
  expect_equal(test_tab, ctrl_tab)
})

test_that('tables with firstonly are properly handled', {
  expect_message(tmp_table <- read_nm_tables(firstonly_test, quiet = FALSE), regexp = 'Consolidating|Joining')
  skip_on_cran()
  expect_equal(tmp_table$data[[1]], 
               xpdb_ex_pk$data$data[[1]][, unlist(xpdb_ex_pk$data$index[[1]][xpdb_ex_pk$data$index[[1]]$table %in% c('sdtab001', 'patab001'),]$col) ])
})

test_that('properly assign skip and header arguments', {
  files <- tempfile(c('tmp_a', 'tmp_b'))
  on.exit(unlink(files))
  
  # Test with skip = 1 and header = TRUE
  writeLines(text = test_file[1:4], con = files[1])
  expect_equal(read_nm_tables(file = files[1], quiet = TRUE),  ctrl_file)
  
  # Test with skip = 0 and header = TRUE
  writeLines(text = test_file[2:4], con = files[2])
  expect_equal(read_nm_tables(file = files[2], quiet = TRUE),  ctrl_file)
})

test_that('properly pick us column signs', {
  files <- tempfile('tmp_c')
  on.exit(unlink(files))
  writeLines(text = minus_sign_test, con = files)
  
  # Positive control with readr (reproducible bug example)
  expect_false(min(readr::read_table(file = files, skip = 2, guess_max = 1,
                                     col_names = c('ID', 'TIME', 'DV'))$DV) == -5)
  
  # Text on xpose
  expect_true(min(read_nm_tables(file = files, quiet = TRUE, 
                                 guess_max = 1, skip = 2)$DV) == -5)
  })

