#' Draw an xpose_plot object
#' 
#' @description This function explicitly draw an xpose_plot and interprets keywords 
#' contained in labels.
#' 
#' @param x An \code{xpose_plot} object.
#' @param page The page number to be drawn. Can be specified as vector or range 
#' of integer values.
#' @param ... Options to be passed on to the ggplot2 print method.
#' 
#' @method print xpose_plot
#' @examples
#' my_plot <- dv_vs_ipred(xpdb_ex_pk) +
#'             labs(title = 'A label with keywords: @nind individuals & @nobs observations')

#' # Using the print function
#' print(my_plot)
#' 
#' # Or simply by writting the plot object name
#' my_plot
#' 
#' @export
print.xpose_plot <- function(x, page, ...) {
  
  # Parse template titles
  if (is.xpose.plot(x)) {
    var_map <- as.character(x$mapping)
    x$labels$title <- append_suffix(x$xpose, x$labels$title, 'title')
    x$labels$subtitle <- append_suffix(x$xpose, x$labels$subtitle, 'subtitle')
    x$labels$caption  <- append_suffix(x$xpose, x$labels$caption, 'caption')
    x$labels <- x$labels %>% 
      purrr::map_if(stringr::str_detect(., '@'),
                    .f = parse_title, xpdb = x$xpose,
                    problem = x$xpose$problem, quiet = x$xpose$quiet,
                    ignore_key = c('page', 'lastpage'),
                    extra_key = c('plotfun', 'timeplot', names(var_map)), 
                    extra_value = c(x$xpose$fun, 
                                    format(Sys.time(), "%a %b %d %X %Z %Y"), 
                                    var_map))
  }
  
  # Print multiple pages
  if (class(x$facet)[1] %in% c('FacetWrapPaginate', 'FacetGridPaginate')) {
    
    # Get total number of pages
    page_tot <- n_pages(repair_facet(x))
    
    # Get and check the page number to be drawn
    if (!missing(page)) {
      page_2_draw <- page
    } else if (!is.null(x$facet$params$page)) {
      page_2_draw <- x$facet$params$page
    } else {
      page_2_draw <- 1:page_tot
    }
    
    if (any(page_2_draw > page_tot)) {
      page_2_draw <- page_2_draw[page_2_draw <= page_tot]
      if (length(page_2_draw) == 0) {
        stop('All `page` element exceeded the total (', page_tot, ') number of pages.', call. = FALSE)  
      }
      warning('`page` contained elements exceeding the total (', page_tot, ') number of pages. These were ignored.',
              call. = FALSE)
    }
    
    # Prevent issue with facet_repair when page = NULL
    x$facet$params$page <- page_2_draw
    
    # Begin multiple page ploting
    n_page_2_draw <- length(page_2_draw)
    
    if (interactive() && !x$xpose$quiet) {
      message('Rendering ', n_page_2_draw, ' selected page(s) out of ', page_tot, '.')
    }
    
    if (n_page_2_draw == 1) {
      x %>% 
        paginate(page_2_draw, page_tot) %>% 
        repair_facet() %>% 
        print.ggplot(...)
    } else {
      if (interactive() && !x$xpose$quiet) {
        pb <- utils::txtProgressBar(min = 0, max = n_page_2_draw, 
                                    style = 3)   # Create progress bar
      }
      for (p in seq_along(page_2_draw)) {
        x$facet$params$page <- page_2_draw[p]
        x %>% 
          paginate(page_2_draw[p], page_tot) %>% 
          repair_facet() %>% 
          print.ggplot(...)
        if (interactive() && !x$xpose$quiet) {
          utils::setTxtProgressBar(pb, value = p) # Update progress bar
        }
      }
      if (interactive() && !x$xpose$quiet) close(pb)
      
      # Prevent ggforce from droping multiple pages value
      x$facet$params$page <- page_2_draw
    }
  } else {
    if (!missing(page)) warning('Faceting not set. Ignoring `page` argument.', call. = FALSE)
    
    # Warn for big plots
    panel_tot <- n_panels(x)
    
    if (panel_tot > 20) {
      msg(c('The faceting resulted in ', panel_tot, 
            ' panels. The plot may take a while to render.'), 
          quiet = x$xpose$quiet)
    }
    
    # Print without multiple pages
    x %>% 
      paginate(page_2_draw = 1, page_tot = 1) %>% 
      repair_facet() %>% 
      print.ggplot(...)
  }
}

# Import print ggplot method
print.ggplot <- get('print.ggplot', envir = asNamespace('ggplot2'))


# Add page number to pages
paginate <- function(plot, page_2_draw, page_tot) {
  plot$labels <- plot$labels %>% 
    purrr::map_if(stringr::str_detect(., '@(page|lastpage)'),
                  .f = parse_title, xpdb = plot$xpose,
                  problem = plot$xpose$problem, quiet = plot$xpose$quiet,
                  extra_key = c('page', 'lastpage'), 
                  extra_value = c(as.character(page_2_draw), page_tot))
  plot
}


# Temporary fix for ggforce facet_wrap_paginate (may)
repair_facet <- function(x) {
  if (class(x$facet)[1] == 'FacetWrapPaginate' && 
      !'nrow' %in% names(x$facet$params)) {
    x$facet$params$nrow <- x$facet$params$max_row
  }
  x
}

# Calculate the total number of pages
n_pages <- function(plot) {
  if (utils::packageVersion('ggplot2') <= '2.2.1') {
    page <- ggplot_build(plot)$layout$panel_layout$page
  } else {
    page <- ggplot_build(plot)$layout$layout$page
  }
  if (!is.null(page)) {
    max(page)
  } else {
    0L
  }
}

# Calculate the total number of panels
n_panels <- function(plot) {
  if (utils::packageVersion('ggplot2') <= '2.2.1') {
    page <- ggplot_build(plot)$layout$panel_layout
  } else {
    page <- ggplot_build(plot)$layout$layout
  }
  if (!is.null(page)) {
    nrow(page)
  } else {
    0L
  }
}
