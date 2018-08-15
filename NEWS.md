# xpose 0.4.2
### General
* Address CRAN requests
* Improved internal structure of VPC
* Fixed bug with vpc 1.0.1'
* Allow for remapped column names in model file (like ID=SUBJID)

# xpose 0.4.1
### General
* Improved integration of the xpose.nlmixr package
* Prevented a problem in the `print()` function with the upcoming ggplot2 version
* Fixed bug in `summary()` where missing estimation method was reported if NM code written as METH=... instead of METHOD=...
* Fixed bug where labels in `prm_table()`, `get_prm()` were missing with a commented row in $THETA, $OMEGA or $SIGMA
* Fixed bug in `prm_table()`, `get_prm()` where only `NA` would be reported when missing the -1000000006 record in the .ext file (i.e. NM <7.3)
* Prevented negative RSE in `prm_table()` and `get_prm()`
* Improved description of the `prm_table()` output
* Fixed bug in `summary()` with non numeric covariance step time
* Fixed bug in `ind_plots()` where the aesthetics would get mixed up if the variable names were changed
* Small fixes to vignettes, documentations and website


# xpose 0.4.0
### General
* Added xpdb memory usage to `print.xpose_data()`
* Replaced `print.prm.data()` to `prm_table()`
* Fixed documentation

### Data import/edit
* Added compatibility with `dplyr::n()` when editing xpdb #51
* Added `get_special()` to access special data
* Improved `get_prm()` labeller parsing with OMEGA and SIGMA BLOCKS
* `get_prm()` and `prm_table()`:
    - Both gained a `transform` argument to disable parameter transformation
    - RSE is now always reported (also for untransformed)
    - Corrected RSE for off-diagonal elements from OMEGA and SIGMA when output as correlation and SE when output as standard deviation
* Added `list_data()`, `list_files()` and `list_special()` to get info on the data structure in the xpdb
* Renamed the arguments `problem`, `subprob`, `method` and `source` to `.problem`, `.subprob`, etc. for consistency with dplyr functions.
* Improved error checking for themes in `xpose_data`
* Improved error checking in `get_prm()`

### Plots
* `print.xpose_plot()` now displays a message when the number of facets is > 20, informing that many panels are being printed and that it may take a while to render the plot
* Improved `.problem`, `.subprob` and `.method` error checking

# xpose 0.3.0
### General
* Improved documentation and testing
* Improved compatibility with NONMEM 7.4.1
* Improved website look, and content with new 'FAQ' and 'Plot' sections and a cheat-sheet
* Multiple bug small bug fixes and code improvements
* Added multiple page functionality to all plots and `xpose_save()`
* Added `get_prm()` function

### Data import/edit
* Improved `dir` and `file` arguments usage
* Improved error robustness of `xpose_data()` 
* Added new dplyr verbs for xpdb editing: `slice()`, `select()`, `rename()`, `distinct()`, `summarize()`, `group_by()` and `ungroup()`
* dplyr verbs can now also be used to edit vpc data
* Added `irep()` function to add simulation counter to any dataset
* Bug fix in `read_nm_files()` not properly reporting FO method
* Bug fix in `read_nm_tables()` not properly parsing data (missing minus signs) in some very specific cases

### Plots
* Small modifications to `xp_theme()` defaults
* Faceting options can now be defined globally in the `xp_theme`
* Added `amt_vs_idv()` plot
* Improved faceting of minimization plots
* Multiples improvements to the `vpc_data()` and `vpc()` functions.
* More keywords added to `template_titles`

# xpose 0.2.0
### General
* Renamed package `xpose`
* Added example dataset `xpdb_ex_pk`
* New internal data structure using nested tibbles
* Improvement of documentation, and testing

### Data import 
#### `read_nm_tables()`
* Handles NONMEM tables in .csv, .zip format
* Handles multiple $PROB and tables with FIRSTONLY option
* Added option to import data manually as in xpose4 with `manual_nm_import()`
* Added indexing of variable and convenience functions to edit it: `set_vars_type()`, `set_vars_label()`, `set_vars_units()`

#### `read_nm_files()`
* Now imports multiple $PROB and sub-problems

### Summary
* Added `print()` and `summary()` methods for xpdb
* Added `list_vars()` function to list available variables
* Added many new keywords to `template_titles`

### Access xpdb
* Added convenience functions to access data from xpdb `get_code()`, `get_data()`, `get_file()`, `get_summary()`.
* Added a method for xpdb to `dplyr::filter()`

### Plots
* Changed `cwres_vs_idv`, type residual functions to more general `res_vs_idv(res = 'CWRES')` functions
* Implemented/improved general plotting functions: `xplot_distrib()`, `xplot_qq()`, `xplot_scatter()`, with convenience function to fetch data in xpdb `data_opt_set()`.
* Implemented: `dv_vs_idv()`, `ipred_vs_idv()`, `pred_vs_idv()`, `dv_preds_vs_idv()`, `ind_plots()`, `vpc()`, `prm_distrib()`, `eta_distrib()`,
`res_distrib()`, `cov_distrib()`, `prm_qq()`, `eta_qq()`,
`res_qq()`, `cov_qq()`, `prm_vs_iteration()`, `grd_vs_iteration()`.
* Updated/renamed: `theme_bw2()`, `theme_readable()`, `theme_xp_default()`, `theme_xp_xpose4()`.

### VPC
* Added wrapper around [`ronkeizer/vpc`](https://github.com/ronkeizer/vpc) inside `vpc_data()`.


# ggxpose 0.1.0
### First commit
* Proof of concept pre-release around simple goodness-of-fit functions like `dv_vs_ipred()`

### Definition of core functions and workflow
* Defined the `xpose_geom()` core function to specifically direct arguments to [`ggplot2`](http://ggplot2.tidyverse.org) layers
* Defined the concept of template titles
* Define the templates for `xpose_theme()`
* Makes use of the [`tidyverse`](http://tidyverse.org) and pipes `%>%`
