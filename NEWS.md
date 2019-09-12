## explore (development version)

* explain_tree(): target can be bin/num/cat
* explain_tree(): add parameter max_target_cat
* explore_shiny(): target can be bin/num/cat
* add function format_num_auto()
* add function total_fig_height()
* get_nrow() deprecated, use total_fig_height() instead
* add parameter title to explore_cor()
* add support for POSIXct in describe()
* update README.md
* update Vignettes
* add NEWS.md
* add hex sticker

## explore 0.4.4 (2019-08-30)

Many functions now accept categorical and numerical targets (next to a binary target). If you want to force which geom is used for visualisation, you can use explore_bar() and explore_density(). New function explore_tbl() to visualise a dataframe/table (type of variables, number of NA, ...)

* add function explore_bar()
* explore_density() now using correct tidy eval, target cat > 2 possible
* target_explore_cat() now using correct tidy eval
* target_explore_num() now using correct tidy eval
* add plot_var_info() - plots a info-text to a variable as ggplot obj.
* plot_var_info() used in explore/explore_all if <oth>
* add parameter max_cat in explore_bar(), explore_density() and explain_tree()
* add explore_tbl()
* drop explore_cat() & explore_num()
* rename template_report_target_den.html > _split.html
* intelligent placing of labels in plots
* add info window "generating report ..." in explore_shiny
* format_num() -> format_num_kMB(), format_num_space()
* format_target -> if numeric split 0/1 by mean
* report() -> default .html file extension
* consistency showing NA info in explore-title
* parameter split: default = FALSE
* allow numeric (num) target in explore_all & report
* describe_tbl() -> fix target if not bin
* desribe(): change out="vector" to out="list"

## explore 0.4.3 (2019-06-20)

* fix parameter in explore: auto_scale, na
* fix number of NA in explore (move code before auto_scale)
* explore_density() with target: drop plot title "propensity by"
* explore_shiny(): use output_dir / tempdir()
* change naming "attribute" to "variable" (consistent)
