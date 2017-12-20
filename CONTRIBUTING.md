# Contributing to xpose

We are happy that you are willing to help us out with the development of xpose. Before get started please take some time to read the following guidelines. 

## Submitting issues

Before posting a new issue, please check that it has not already been posted.

* For **feature requests** make sure to include sufficient information, such as a proposed workflow, plots images, links to posters or publications describing a method, etc.
* For **reporting a bug**, the most important thing is to include a minimal reproducible example so that we can quickly verify your problem, and figure out how to fix it. To make your example reproducible:

    + Attach files such as the NONMEM output files or an xpdb saved as *.Rdata* (i.e. `save(xpdb, "xpdb_error.Rdata")`). Note, you can use the arguments `ignore` and/or `extra_files` of `xpose_data()` to reduce the size of the xpdb whenever needed.
    + Include the R code that created the error. Ensure that your code is clear, well commented and as small as necessary to reproduce the error.
    + Include the error message.
    + Include information on your R session using `sessionInfo()`.

## Submitting pull requests

To contribute code to xpose please follow these steps listed below. Note: if you are unfamiliar with the development of R packages, please take some time to read the [R packages](http://r-pkgs.had.co.nz) book by Hadley Wickham.

1. [Fork](https://github.com/UUPharmacometrics/xpose/fork) the xpose repository.
1. Make the changes to the code on your fork.

    + Use the [coding style](http://r-pkgs.had.co.nz/style.html) recommended by Hadley Wickham.
    + Any new function should have [Roxygen](http://r-pkgs.had.co.nz/man.html) documentation. Internal functions should contain `#' @keywords internal`. Add examples to new functions. Make sure to update the documentation with `devtools::document()` before submitting the pull request.
    + [Tests](http://r-pkgs.had.co.nz/tests.html) should be added under *tests/testthat/* to ensure for proper code coverage.
    + New plots should be created using the generic `xplot_scatter()`, `xplot_distrib()` or `xplot_qq()`. Check for example the code of *R/plot_gof.R* to see how it should be done.
    + General utility functions should be added under *R/utils.R* and plot utility functions under *R/xplot_helpers.R*.
    + Use functions from the tidyverse (e.g. dplyr, tidyr, stringr, etc.) whenever possible.
    + Add a brief description of your contribution to *NEWS.md* using bullets. Make sure to list it under the most recent xpose version and to add your name as (`@<your_github_username>`). e.g. `* Added sharkplot (@guiastrennec)`.
    
1. Run `devtools::check()` on your fork and ensure the absence of any errors, warnings and notes.
1. Create a pull request to the **dev branch** of the **UUPharmacometrics/xpose** repository.

    + Provide a good description of your pull request. Add examples, function outputs and plots images illustrating the new features.
    + Make sure that travis-ci, appveyor and codecov tests are all sucessfull
    
1. Address all comments until the pull request is either merged or closed.

All of this may seem complicated at first, but contact one of the core developers to assist you.
