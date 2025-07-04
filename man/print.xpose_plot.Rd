% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_xpose_plot.R
\name{print.xpose_plot}
\alias{print.xpose_plot}
\title{Draw an xpose_plot object}
\usage{
\method{print}{xpose_plot}(x, page, ...)
}
\arguments{
\item{x}{An \code{xpose_plot} object.}

\item{page}{The page number to be drawn. Can be specified as vector or range 
of integer values.}

\item{...}{Options to be passed on to the ggplot2 print method.}
}
\description{
This function explicitly draw an xpose_plot and interprets keywords 
contained in labels.
}
\examples{
my_plot <- dv_vs_ipred(xpdb_ex_pk) +
            labs(title = 'A label with keywords: @nind individuals & @nobs observations')
# Using the print function
print(my_plot)

# Or simply by writing the plot object name
my_plot

}
