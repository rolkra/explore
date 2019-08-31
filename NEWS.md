## explore (development version)

* update README.md
* add NEWS.md
* add hex sticker

## explore 0.4.4 (2019-08-30)

* add function explore_bar
* explore_density now using correct tidy eval, target cat > 2 possible
* target_explore_cat now using correct tidy eval
* target_explore_num now using correct tidy eval
* add plot_var_info() - plots a info-text to a variable as ggplot obj.
* plot_var_info used in explore/explore_all if <oth>
* add parameter max_cat in explore_bar, explore_density and explain_tree
* add explore_tbl()
* drop explore_cat() & explore_num()
* rename template_report_target_den.html > _split.html
* intelligent placing of labels in plots
* add info window "generating report ..." in explore_shiny
* format_num -> format_num_kMB, format_num_space
* format_target -> if numeric split 0/1 by mean
* report -> default .html file extension
* consistency showing NA info in explore-title
* parameter split: default = FALSE
* allow numeric (num) target in explore_all & report
* describe_tbl -> fix target if not bin
* desribe: change out="vector" to out="list"
