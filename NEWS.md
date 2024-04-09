# explore 1.3.0

## New features

* add parameter `nthread` to `explain_xgboost()`. (#45)
* add `interact()`. (#47)
* add interactivity as default in explore shiny app. (#47)
* add `create_data_abtest()`.
* add basic color parameter to `explore()` & `abtest()` functions.
* add `get_color()`
* move NA-info in `explore()` from title to subtitle. (#48)
* add more descriptive infos into `explore()`subtitle. 
* add `color` parameter for `explore()`, `explore_*()`, `report()`
* add `bins` parameter to `target_explore_num()`

## Breaking changes

* `mix_color()` with one color as parameter generates colors from light to dark 
* `target_explore_num()` bar positioning changes from max to mean value

## Bug fixes / internal

* fix undefined / not meaningful values in abtest() shiny-app

# explore 1.2.0

## New features

* add `explain_xgboost()` (#42)
* add `drop_var_by_names()` (#43)
* add `drop_var_not_numeric()` (#43)
* add `drop_var_low_variance()` (#43)
* add `drop_var_no_variance()` (#43)
* add `drop_var_with_na()` (#43)
* add `drop_obs_with_na()` (#43)
* add `drop_obs_if()` (#43)
* add `mix_color()`
* add `show_color()` 
* add `create_data_esoteric()`

## Breaking changes

* `create_data_empty()` has no longer a parameter `seed` 

## Bug fixes / internal

* add `check_vec_low_variance()` (internal helper function)

# explore 1.1.1

* Fix CRAN check warning in running example `get_nrow()` (#41)

# explore 1.1.0

## Breaking changes

* explore no longer imports MASS and RandomForest. for `explain_logreg()` and `explain_forest()`, you will receive a prompt to install these packages in interactive sessions. (#2 1, @olivroy)

## New features

* add `explain_forest()`.
* add `predict_target()`.
* add `create_data_newsletter()`.
* add `use_data_beer()` and `use_data_starwars()` functions (#20, #23)
* explore now has a [website](https://rolkra.github.io/explore/). (#17, #19, @olivroy)
* `abtest()` now supports numeric target (t-test).
* `abtest_targetpct()` with count data (parameter `n`).
* `abtest()` and `explore()` can now run without data (shiny app). If no data are provided, `palmerpenguins::penguins` is used. (#25)
* New vignettes were added. (#27, #28, #29, #31)
* Documentation and examples were enhanced. (#38, @olivroy, #32, #33, #36, #37)
* `create_data_()` `use_data_*()` return data sets as tibble.

## Bug fixes / internal

* drop use of `fct_explicit_na()` (forcats >= 1.0.0) and use `linewidth` for ggplot2 (>= 3.4.0) (deprecated) (#15, @olivroy)
* explore no longer depends on assertthat, tidyr, and broom. (#16, #21, @olivroy).
* Error messages are now more informative (#18, @olivroy)
* explore now uses testthat 3rd edition and GitHub actions (#16, @olivroy)
* explore has been re-licensed as MIT (#22)

# explore 1.0.2 (2023-01-14)

* `add_var_random_01()` creates variable of type integer
* add `target_name` & `factorise_target` parameter to more `create_data_*()`
* add `target1_prob` parameter to more `create_data_*()`
* add checks to `create_data_*()`
* format variable random_moon
* add `abtest()`
* remove native pipe in `explore_tbl()`
* fix error in `explore()` median if `NA` values
* add tests for `explore()` (no error if data contains `NA`)

# explore 1.0.1 (2022-12-20)

* Switch back to `%>%` in vignettes (compatibility R < 4.1)  (#6)
* No hard coded path for markdown-templates
* Add `create_data_unfair()`
* `create_data_app()` gains a `screen_size` argument. 

# explore 1.0.0 (2022-11-11)

* Dependency DT (>= 0.3.0)
* Improve and use native pipe in README.
* Improve documentation
* Add function `create_data_app()`
* Add support for integer64
* Bugfix `report()` >100 variables
* No warning-message in `explore_count()`
* Redesign `explore_tbl()`
* Add mean to `explore_density()` plot
* Add `create_data_churn()`
* Add `add_var_random_moon()`
* Vignettes: switch from `%>%` to `|>`
* Add `create_notebook_explore()`

# explore 0.9.0 (2022-08-31)

* rename create_x_data() to `create_data_x()`
* rename add_x_var() to `add_var_x()`
* extend `create_data_*()` functions
* extend `add_var_*()` functions
* `explain_tree()`: set default `minsplit = 20`
* `explain_tree()`: set prior probabilities
* `explore()` and `report()`: `targetpct` as alternative to `split` parameter
* `balance_target()`: add parameter seed
* fix variable type for `create_data_x()`
* using md in roxygen

# explore 0.8.0 (2022-01-30)

* all `dwh_*()` functions are no longer included in {explore}
  Alternative: source https://github.com/rolkra/dwh
* add `create_fake_data()`
* add `create_random_data()`
* add `add_random_var()`
* add `get_var_buckets()`
* `total_fig_height()`: parameters `var_name_target`, `var_name_n` 
* code styling
* report templates: add var buckets (to plot large number of variables)

# explore 0.7.1 (2021-06-04)

* change `theme_light()` into `individual theme()` so that `set_theme` works.
* add rmarkdown to Suggests in DESCRIPTION
* fix URL in DESCRIPTION
* drop LazyData in DESCRIPTION
* format DESCRIPTION

# explore 0.7.0 (2021-01-21)

* `explain_tree()` gains a `weights` parameter.
* flip no/yes label in rpart.plot
* fix `minsplit` for count-data
* add `weight_target()`
* add `plot_legend_targetpct()`
* add legend in targetpct rmarkdown-template
* add unit testing (testthat)

# explore 0.6.2 (2020-10-14)

* Bugfix `explore_bar()`: `NA` in plot
* `explore_count()`: convert target into factor
* `explore_count()`: add default title (cat name)
* `explore_count()`: add parameter numeric, max_cat, max_target_cat
* `explain_tree()`: convert character variables into factors (count data)
* `explain_tree()`: parameter out ("plot" | "model")
* `explain_logreg()`: parameter out ("tibble" | "model")
* `vignette("explore_titanic")`: change to tibble
* `vignette("explore_mtcars")`: add explanations
* change theme_minimal to theme_light
* dwh_fastload(): add parameters overwrite and append
* update README.md

# explore 0.6.1 (2020-09-04)

* Fix Github URL
* new Vignette `vignette("explore_penguins")`
* new Vignette `vignette("explore_titanic")` (count data)
* `explore_count()`: plot count() output
* add default parameter `n` for count data: 
  `explore()`, `explore_all()`, `explore_tbl()`, 
  `explain_tree()`, `report()`,
  `describe()`, `describe_cat()`, `describe_num()`, `describe_tbl()`,
  `total_fig_height()`
* `explore_tree()`: default value for minsplit = 10% of obs
* `explore_cor()`: use `geom_point()` for small datasets
* `explore_shiny()`: use `browseURL()` with parameter `browser=NULL`
* `describe_tbl()`: add observations containing `NA`
* `guess_cat_num()`: parameter description (optional)
* `count_pct()`: no renaming of variables.


# explore 0.5.5 (2020-04-06)

Maintenance update:

* fix breaking changes tibble 3.0.0


# explore 0.5.4 (2020-02-09)

Maintenance update:

* fix param `...` in description (PR#16223, see
<https://bugs.r-project.org/show_bug.cgi?id=16223>)


# explore 0.5.3 (2020-01-17)

* `explore_bar()`: add parameter numeric
* `describe_all()` returns a tibble
* `describe_all()`: column 'variable' is character (not factor)
* `report()` split = TRUE as default
* add `rescale01()`
* add parameter `rescale01` to `clean_var()`
* add function `count_pct()`
* add `out='tibble'` to `describe_cat()`
* add function `explore_targetpct()`

# explore 0.5.2 (2019-11-22)

* split source-code file into multiple files
* `format_num_auto()` without brackets
* treat Date variables as cat
* `report()` fix automatic file extension .html
* add `simplify_text()`
* add parameter `simplify_text` to `clean_var()`
* fix link in README.md

# explore 0.5.1 (2019-10-08)

Prepare for new dplyr 0.8.4 (#2, @romainfrancois)

## Bug Fixes

* prepare `explore_tbl()` for dplyr 0.8.4
* `describe_num()` with default digits=6
* `describe_cat()` bugfix variable with all NA
* `describe_all()` bugfix variable with all NA
* `explain_tree()` bugfix dataframe with 0 rows
* improve speed `describe()` text output (RMarkdown)
* `explore()` now checks if data is a data.frame

# explore 0.5.0 (2019-09-19)

Interactive data exploration now accept categorical and numerical targets (next to a binary target).

* `explain_tree()`: target can be bin/num/cat
* `explain_tree()`: add parameter max_target_cat
* `explore_shiny()`: target can be bin/num/cat
* add function `format_num_auto()`
* `total_fig_height()` replaces the now deprecated `get_nrow()`.
* add parameter title to `explore_cor()`
* add support for POSIXct in `describe()`
* improved handling of dataframes with no observations
* add parameter `title` to `explore_density()`
* add parameter `nvar` to `total_fig_height()`
* update README.md
* update Vignettes
* add NEWS.md
* add hex sticker

# explore 0.4.4 (2019-08-27)

Many functions now accept categorical and numerical targets (next to a binary target). If you want to force which geom is used for visualisation, you can use explore_bar() and `explore_density()`. New function `explore_tbl()` to visualise a dataframe/table (type of variables, number of NA, ...)

* add function `explore_bar()`
* `explore_density()` now using correct tidy eval, target cat > 2 possible
* `target_explore_cat()` now using correct tidy eval
* `target_explore_num()` now using correct tidy eval
* `add plot_var_info()` - plots a info-text to a variable as ggplot obj.
* `plot_var_info()` used in explore/explore_all if <oth>
* `plot_var_info()` used if explore empty data
* add parameter `max_cat` in `explore_bar()`, `explore_density()` and `explain_tree()`
* add `explore_tbl()`
* drop `explore_cat()` & `explore_num()`
* rename template_report_target_den.html > _split.html
* intelligent placing of labels in plots
* add info window "generating report ..." in `explore_shiny()`
* `format_num()` -> format_num_kMB(), format_num_space()
* `format_target()` -> if numeric split 0/1 by mean
* `report()` -> default .html file extension
* consistency showing NA info in explore-title
* parameter split: default = FALSE
* allow numeric (num) target in explore_all & report
* `describe_tbl()` -> fix target if not bin
* `describe()`: change out="vector" to out="list"

# explore 0.4.3 (2019-06-17)

* fix parameter in `explore()`: `auto_scale`, `na`
* fix number of `NA` in `explore()` (move code before `auto_scale`)
* `explore_density()` with target: drop plot title "propensity by"
* `explore_shiny()`: use output_dir / `tempdir()`
* change naming "attribute" to "variable" (consistent)
