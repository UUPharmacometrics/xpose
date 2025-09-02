# Define plots to be tested -----------------------------------------------

# General tests
p1 <- xplot_scatter(xpdb = xpdb_ex_pk, aes(x = .data[["PRED"]], y = .data[["DV"]]), guide = TRUE, type = 'plst',
                    title = '@run-title', subtitle = '@run-subtitle', caption = '@run-caption',
                    tag = '@run-tag', point_color = 'red', line_color = 'blue', 
                    smooth_color = 'green', text_color = 'yellow', quiet = TRUE)
d1 <- xplot_distrib(xpdb = xpdb_ex_pk, aes(x = .data[["PRED"]]), guide = FALSE, type = 'dhr',
                    title = '@run-title', subtitle = '@run-subtitle', caption = '@run-caption',
                    tag = '@run-tag', histogram_color = 'red', density_color = 'blue',
                    rug_color = 'yellow', quiet = TRUE)
q1 <- xplot_qq(xpdb = xpdb_ex_pk, aes(sample = .data[["PRED"]]), guide = TRUE, type = 'p',
               title = '@run-title', subtitle = '@run-subtitle', caption = '@run-caption',
               tag = '@run-tag', point_color = 'red', guide_color = 'blue', quiet = TRUE)

# Facet wrap
p2 <- xplot_scatter(xpdb = xpdb_ex_pk, aes(x = .data[["PRED"]], y = .data[["DV"]]), 
                    facets = c('MED2', 'SEX'), quiet = TRUE)
d2 <- xplot_distrib(xpdb = xpdb_ex_pk, aes(x = .data[["PRED"]]), 
                    facets = c('MED2', 'SEX'), quiet = TRUE, page = 1, ncol = 1, nrow = 1)
q2 <- xplot_qq(xpdb = xpdb_ex_pk, aes(sample = .data[["PRED"]]), 
               facets = c('MED2', 'SEX'), quiet = TRUE, page = 1:2, ncol = 1, nrow = 1)

# Facet grid
p3 <- xplot_scatter(xpdb = xpdb_ex_pk, aes(x = .data[["PRED"]], y = .data[["DV"]]), 
                    facets = MED2~SEX, quiet = TRUE, page = 1, ncol = 1, nrow = 1)
d3 <- xplot_distrib(xpdb = xpdb_ex_pk, aes(x = .data[["PRED"]]), 
                    facets = MED2~SEX, quiet = TRUE)
q3 <- xplot_qq(xpdb = xpdb_ex_pk, aes(sample = .data[["PRED"]]), 
               facets = MED2~SEX, quiet = TRUE, page = 1:2, ncol = 1, nrow = 1)


# Tests start here --------------------------------------------------------

test_that('errors are returned for missing or bad xpdb', {
  expect_error(xplot_scatter())
  expect_error(xplot_distrib())
  expect_error(xplot_qq())
  
  expect_error(xplot_scatter(xpdb = 1), regexp = 'Bad input')
  expect_error(xplot_distrib(xpdb = 1), regexp = 'Bad input')
  expect_error(xplot_qq(xpdb = 1), regexp = 'Bad input')
})

test_that('xpose plot objects are returned with appropriate xpdb', {
  # Check class
  expect_true(is.xpose.plot(p1))
  expect_true(is.xpose.plot(d1))
  expect_true(is.xpose.plot(q1))
  
  # Check metadata
  expect_equal(p1$xpose$fun, 'scatter_plot')
  expect_equal(p1$xpose$summary, xpdb_ex_pk$summary)
  expect_equal(p1$xpose$problem, 1)
  expect_true(p1$xpose$quiet)
  
  expect_equal(d1$xpose$fun, 'density_plot')
  expect_equal(d1$xpose$summary, xpdb_ex_pk$summary)
  expect_equal(d1$xpose$problem, 1)
  expect_true(d1$xpose$quiet)
  
  expect_equal(q1$xpose$fun, 'qq_plot')
  expect_equal(q1$xpose$summary, xpdb_ex_pk$summary)
  expect_equal(q1$xpose$problem, 1)
  expect_true(q1$xpose$quiet)
})

test_that('layers of xplot_scatter', {
  expect_equal(class(p1$layers[[1]]$geom)[1], 'GeomLine')
  expect_equal(class(p1$layers[[2]]$geom)[1], 'GeomPoint')
  expect_equal(class(p1$layers[[3]]$geom)[1], 'GeomText')
  expect_equal(class(p1$layers[[4]]$geom)[1], 'GeomAbline')
  expect_equal(class(p1$layers[[5]]$geom)[1], 'GeomSmooth')
})

test_that('layers of xplot_density', {
  expect_equal(class(d1$layers[[1]]$geom)[1], 'GeomBar')
  expect_equal(class(d1$layers[[2]]$geom)[1], 'GeomDensity')
  expect_equal(class(d1$layers[[3]]$geom)[1], 'GeomRug')
})

test_that('layers of xplot_qq', {
  expect_equal(class(q1$layers[[1]]$geom)[1], 'GeomPoint')
  
  ### TEMPORARY: Ignore test for older ggplot2 versions ############
  expect_equal(class(q1$layers[[2]]$geom)[1], ifelse(packageVersion("ggplot2") > "3.5.2", 'GeomAbline', "GeomPath"))
  # expect_equal(class(q1$layers[[2]]$geom)[1], 'GeomAbline')
  ##################################################################
  
})


test_that('faceting works properly', {
  expect_true(is.null(p1$facet$params$facets))
  expect_true(is.null(d1$facet$params$facets))
  expect_true(is.null(q1$facet$params$facets))
  
  expect_true(is.list(p2$facet$params$facets))
  expect_true(is.list(d2$facet$params$facets))
  expect_true(is.list(q2$facet$params$facets))
  
  expect_equal(rlang::quo_name(p3$facet$params$cols[[1]]), 'SEX')
  expect_equal(rlang::quo_name(p3$facet$params$rows[[1]]), 'MED2')
  expect_equal(rlang::quo_name(d3$facet$params$cols[[1]]), 'SEX')
  expect_equal(rlang::quo_name(d3$facet$params$rows[[1]]), 'MED2')
  expect_equal(rlang::quo_name(q3$facet$params$cols[[1]]), 'SEX')
  expect_equal(rlang::quo_name(q3$facet$params$rows[[1]]), 'MED2')
})

test_that('xpose_geom mapping works properly', {
  expect_equal(p1$layers[[2]]$aes_params$colour, 'red')
  expect_equal(p1$layers[[3]]$aes_params$colour, 'yellow')
  expect_equal(p1$layers[[4]]$aes_params$colour, 'grey70')
  expect_equal(p1$layers[[5]]$aes_params$colour, 'green')
  
  expect_equal(d1$layers[[1]]$aes_params$colour, 'red')
  expect_equal(d1$layers[[2]]$aes_params$colour, 'blue')
  expect_equal(d1$layers[[3]]$aes_params$colour, 'yellow')
  
  expect_equal(q1$layers[[1]]$aes_params$colour, 'red')
  expect_equal(q1$layers[[2]]$aes_params$colour, 'blue')
})
