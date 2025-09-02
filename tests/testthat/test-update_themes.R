# Define custom theme -----------------------------------------------------
theme_xp_custom <- theme_xp_default()
theme_xp_custom$point_color <- 'green'
theme_xp_custom <- as.xpose.theme(theme_xp_custom)


# Tests start here --------------------------------------------------------

test_that('returns error on missing xpdb', {
  expect_error(update_themes())
})

test_that('returns identical xpdb if missing themes', {
  expect_identical(update_themes(xpdb = xpdb_ex_pk), xpdb_ex_pk)
})

test_that('full gg_themes are properly added', {
  expect_equal(ignore_attr = FALSE,
               update_themes(xpdb = xpdb_ex_pk, 
                             gg_theme = theme_bw2())$gg_theme, 
               theme_bw2(),
               check.attributes = FALSE,
               check.environment = FALSE
  )
})

test_that('message returned on bad gg_theme input', {
  expect_message(update_themes(xpdb = xpdb_ex_pk, 
                               gg_theme = theme_xp_default(),
                               quiet = FALSE))
})

test_that('full xp_themes are properly added', {
  expect_equal(update_themes(xpdb = xpdb_ex_pk,
                             xp_theme = theme_xp_xpose4())$xp_theme,
               theme_xp_xpose4(),
               check.attributes = FALSE,
               check.environment = FALSE)
})

test_that('parial xp_themes are properly added', {
  ### TEMPORARY: Ignore test for older ggplot2 versions ############
  skip_if_not_installed(pkg = "ggplot2", minimum_version = "4.0.0")
  ##################################################################
  
  test <- update_themes(xpdb = xpdb_ex_pk, xp_theme = c(point_color = 'green'))$xp_theme
  test_class <- class(test)
  test$labeller <- NULL ## Reduce sensibility to ggplot2 updates
  class(test) <- test_class
  
  ref <- theme_xp_custom
  ref_class <- class(ref)
  ref$labeller  <- NULL  ## Reduce sensibility to ggplot2 updates
  class(ref) <- ref_class
  attr(ref, "theme") <- "theme_xp_default (modified)"
  
  expect_equal(test, ref)
})

test_that('message on unnamed xp_theme input', {
  expect_message(update_themes(xpdb = xpdb_ex_pk, 
                               xp_theme = c('blue', 'dashed'), 
                               quiet = FALSE))
})
