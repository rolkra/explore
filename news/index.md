# Changelog

## explore 1.4.0

### Bug fixes / internal

- prepare for breaking changes in {xgboost} 3.1.2.1 (CRAN release
  2025-12-03)

## explore 1.3.6

CRAN release: 2025-10-02

### new features

- fix unique values in subheader of plot
  [`explore_bar()`](../reference/explore_bar.md) (not limited by
  max_cat)
- fix unique values in subheader of plot
  [`explore_count()`](../reference/explore_count.md) (not limited by
  max_cat)
- [`explore_tbl()`](../reference/explore_tbl.md) now has centered labels

## explore 1.3.5

CRAN release: 2025-06-24

### New features

- add new parameter `short_names` for
  [`use_data_penguins()`](../reference/use_data_penguins.md)
- add new parameter `diff_to` for
  [`yyyymm_calc()`](../reference/yyyymm_calc.md)

## explore 1.3.4

CRAN release: 2025-03-30

### New features

- add new data for
  [`use_data_wordle()`](../reference/use_data_wordle.md)

### Bug fixes / internal

- round absolute numbers if [`abtest()`](../reference/abtest.md) is used
  with percentage
- accept vector as input for [`mix_color()`](../reference/mix_color.md),
  but only use first element
- `geom_abline()`: switch from `size` to `linewidth`

## explore 1.3.3

CRAN release: 2024-11-12

### New features

- add [`explore_col()`](../reference/explore_col.md) for simple bar
  plots without aggregation
- add [`yyyymm_calc()`](../reference/yyyymm_calc.md) for calculation
  with periods (format yyyymm)
- add [`use_data_wordle()`](../reference/use_data_wordle.md): data from
  a wordle challange
- update vignette `abtest.Rmd`

### Bug fixes / internal

- fix create_data_abtest() with success_unit = “percent”

## explore 1.3.2

CRAN release: 2024-09-02

### New features

- change default color from grey to “#ADD8E6”, “#7BB8DA”
- add adaptive border color to geom_point plots (correlation x,y)
- new adaptive color for mean & regression line

### Bug fixes / internal

- drop CRAN download statistics

## explore 1.3.1

CRAN release: 2024-06-07

### New features

- add correlation to [`explore_cor()`](../reference/explore_cor.md) when
  using `geom_points`

## explore 1.3.0

CRAN release: 2024-04-15

### New features

- add parameter `nthread` to
  [`explain_xgboost()`](../reference/explain_xgboost.md).
  ([\#45](https://github.com/rolkra/explore/issues/45))
- add [`interact()`](../reference/interact.md).
  ([\#47](https://github.com/rolkra/explore/issues/47))
- add interactivity as default in explore shiny app.
  ([\#47](https://github.com/rolkra/explore/issues/47))
- add [`create_data_abtest()`](../reference/create_data_abtest.md).
- add basic color parameter to [`explore()`](../reference/explore.md) &
  [`abtest()`](../reference/abtest.md) functions.
- add [`get_color()`](../reference/get_color.md)
- move NA-info in [`explore()`](../reference/explore.md) from title to
  subtitle. ([\#48](https://github.com/rolkra/explore/issues/48))
- add more descriptive infos into
  [`explore()`](../reference/explore.md)subtitle.
- add `color` parameter for [`explore()`](../reference/explore.md),
  `explore_*()`, [`report()`](../reference/report.md)
- add `bins` parameter to
  [`target_explore_num()`](../reference/target_explore_num.md)

### Breaking changes

- [`mix_color()`](../reference/mix_color.md) with one color as parameter
  generates colors from light to dark
- [`target_explore_num()`](../reference/target_explore_num.md) bar
  positioning changes from max to mean value

### Bug fixes / internal

- fix undefined / not meaningful values in abtest() shiny-app
- rename vignettes `explore_*.Rmd` to `explore-*.Rmd`

## explore 1.2.0

CRAN release: 2024-02-28

### New features

- add [`explain_xgboost()`](../reference/explain_xgboost.md)
  ([\#42](https://github.com/rolkra/explore/issues/42))
- add [`drop_var_by_names()`](../reference/drop_var_by_names.md)
  ([\#43](https://github.com/rolkra/explore/issues/43))
- add [`drop_var_not_numeric()`](../reference/drop_var_not_numeric.md)
  ([\#43](https://github.com/rolkra/explore/issues/43))
- add [`drop_var_low_variance()`](../reference/drop_var_low_variance.md)
  ([\#43](https://github.com/rolkra/explore/issues/43))
- add [`drop_var_no_variance()`](../reference/drop_var_no_variance.md)
  ([\#43](https://github.com/rolkra/explore/issues/43))
- add [`drop_var_with_na()`](../reference/drop_var_with_na.md)
  ([\#43](https://github.com/rolkra/explore/issues/43))
- add [`drop_obs_with_na()`](../reference/drop_obs_with_na.md)
  ([\#43](https://github.com/rolkra/explore/issues/43))
- add [`drop_obs_if()`](../reference/drop_obs_if.md)
  ([\#43](https://github.com/rolkra/explore/issues/43))
- add [`mix_color()`](../reference/mix_color.md)
- add [`show_color()`](../reference/show_color.md)
- add [`create_data_esoteric()`](../reference/create_data_esoteric.md)

### Breaking changes

- [`create_data_empty()`](../reference/create_data_empty.md) has no
  longer a parameter `seed`

### Bug fixes / internal

- add
  [`check_vec_low_variance()`](../reference/check_vec_low_variance.md)
  (internal helper function)

## explore 1.1.1

CRAN release: 2024-02-10

- Fix CRAN check warning in running example
  [`get_nrow()`](../reference/get_nrow.md)
  ([\#41](https://github.com/rolkra/explore/issues/41))

## explore 1.1.0

CRAN release: 2023-10-11

### Breaking changes

- explore no longer imports MASS and RandomForest. for
  [`explain_logreg()`](../reference/explain_logreg.md) and
  [`explain_forest()`](../reference/explain_forest.md), you will receive
  a prompt to install these packages in interactive sessions.
  ([\#2](https://github.com/rolkra/explore/issues/2) 1,
  [@olivroy](https://github.com/olivroy))

### New features

- add [`explain_forest()`](../reference/explain_forest.md).
- add [`predict_target()`](../reference/predict_target.md).
- add
  [`create_data_newsletter()`](../reference/create_data_newsletter.md).
- add [`use_data_beer()`](../reference/use_data_beer.md) and
  [`use_data_starwars()`](../reference/use_data_starwars.md) functions
  ([\#20](https://github.com/rolkra/explore/issues/20),
  [\#23](https://github.com/rolkra/explore/issues/23))
- explore now has a [website](https://rolkra.github.io/explore/).
  ([\#17](https://github.com/rolkra/explore/issues/17),
  [\#19](https://github.com/rolkra/explore/issues/19),
  [@olivroy](https://github.com/olivroy))
- [`abtest()`](../reference/abtest.md) now supports numeric target
  (t-test).
- [`abtest_targetpct()`](../reference/abtest_targetpct.md) with count
  data (parameter `n`).
- [`abtest()`](../reference/abtest.md) and
  [`explore()`](../reference/explore.md) can now run without data (shiny
  app). If no data are provided,
  [`palmerpenguins::penguins`](https://allisonhorst.github.io/palmerpenguins/reference/penguins.html)
  is used. ([\#25](https://github.com/rolkra/explore/issues/25))
- New vignettes were added.
  ([\#27](https://github.com/rolkra/explore/issues/27),
  [\#28](https://github.com/rolkra/explore/issues/28),
  [\#29](https://github.com/rolkra/explore/issues/29),
  [\#31](https://github.com/rolkra/explore/issues/31))
- Documentation and examples were enhanced.
  ([\#38](https://github.com/rolkra/explore/issues/38),
  [@olivroy](https://github.com/olivroy),
  [\#32](https://github.com/rolkra/explore/issues/32),
  [\#33](https://github.com/rolkra/explore/issues/33),
  [\#36](https://github.com/rolkra/explore/issues/36),
  [\#37](https://github.com/rolkra/explore/issues/37))
- `create_data_()` `use_data_*()` return data sets as tibble.

### Bug fixes / internal

- drop use of `fct_explicit_na()` (forcats \>= 1.0.0) and use
  `linewidth` for ggplot2 (\>= 3.4.0) (deprecated)
  ([\#15](https://github.com/rolkra/explore/issues/15),
  [@olivroy](https://github.com/olivroy))
- explore no longer depends on assertthat, tidyr, and broom.
  ([\#16](https://github.com/rolkra/explore/issues/16),
  [\#21](https://github.com/rolkra/explore/issues/21),
  [@olivroy](https://github.com/olivroy)).
- Error messages are now more informative
  ([\#18](https://github.com/rolkra/explore/issues/18),
  [@olivroy](https://github.com/olivroy))
- explore now uses testthat 3rd edition and GitHub actions
  ([\#16](https://github.com/rolkra/explore/issues/16),
  [@olivroy](https://github.com/olivroy))
- explore has been re-licensed as MIT
  ([\#22](https://github.com/rolkra/explore/issues/22))

## explore 1.0.2 (2023-01-14)

CRAN release: 2023-01-14

- [`add_var_random_01()`](../reference/add_var_random_01.md) creates
  variable of type integer
- add `target_name` & `factorise_target` parameter to more
  `create_data_*()`
- add `target1_prob` parameter to more `create_data_*()`
- add checks to `create_data_*()`
- format variable random_moon
- add [`abtest()`](../reference/abtest.md)
- remove native pipe in [`explore_tbl()`](../reference/explore_tbl.md)
- fix error in [`explore()`](../reference/explore.md) median if `NA`
  values
- add tests for [`explore()`](../reference/explore.md) (no error if data
  contains `NA`)

## explore 1.0.1 (2022-12-20)

CRAN release: 2022-12-20

- Switch back to `%>%` in vignettes (compatibility R \< 4.1)
  ([\#6](https://github.com/rolkra/explore/issues/6))
- No hard coded path for markdown-templates
- Add [`create_data_unfair()`](../reference/create_data_unfair.md)
- [`create_data_app()`](../reference/create_data_app.md) gains a
  `screen_size` argument.

## explore 1.0.0 (2022-11-11)

CRAN release: 2022-11-11

- Dependency DT (\>= 0.3.0)
- Improve and use native pipe in README.
- Improve documentation
- Add function [`create_data_app()`](../reference/create_data_app.md)
- Add support for integer64
- Bugfix [`report()`](../reference/report.md) \>100 variables
- No warning-message in
  [`explore_count()`](../reference/explore_count.md)
- Redesign [`explore_tbl()`](../reference/explore_tbl.md)
- Add mean to [`explore_density()`](../reference/explore_density.md)
  plot
- Add [`create_data_churn()`](../reference/create_data_churn.md)
- Add [`add_var_random_moon()`](../reference/add_var_random_moon.md)
- Vignettes: switch from `%>%` to `|>`
- Add
  [`create_notebook_explore()`](../reference/create_notebook_explore.md)

## explore 0.9.0 (2022-08-31)

CRAN release: 2022-08-31

- rename create_x_data() to `create_data_x()`
- rename add_x_var() to `add_var_x()`
- extend `create_data_*()` functions
- extend `add_var_*()` functions
- [`explain_tree()`](../reference/explain_tree.md): set default
  `minsplit = 20`
- [`explain_tree()`](../reference/explain_tree.md): set prior
  probabilities
- [`explore()`](../reference/explore.md) and
  [`report()`](../reference/report.md): `targetpct` as alternative to
  `split` parameter
- [`balance_target()`](../reference/balance_target.md): add parameter
  seed
- fix variable type for `create_data_x()`
- using md in roxygen

## explore 0.8.0 (2022-01-30)

CRAN release: 2022-01-30

- all `dwh_*()` functions are no longer included in {explore}
  Alternative: source <https://github.com/rolkra/dwh>
- add `create_fake_data()`
- add `create_random_data()`
- add `add_random_var()`
- add [`get_var_buckets()`](../reference/get_var_buckets.md)
- [`total_fig_height()`](../reference/total_fig_height.md): parameters
  `var_name_target`, `var_name_n`
- code styling
- report templates: add var buckets (to plot large number of variables)

## explore 0.7.1 (2021-06-04)

CRAN release: 2021-06-04

- change `theme_light()` into `individual theme()` so that `set_theme`
  works.
- add rmarkdown to Suggests in DESCRIPTION
- fix URL in DESCRIPTION
- drop LazyData in DESCRIPTION
- format DESCRIPTION

## explore 0.7.0 (2021-01-21)

CRAN release: 2021-01-21

- [`explain_tree()`](../reference/explain_tree.md) gains a `weights`
  parameter.
- flip no/yes label in rpart.plot
- fix `minsplit` for count-data
- add [`weight_target()`](../reference/weight_target.md)
- add [`plot_legend_targetpct()`](../reference/plot_legend_targetpct.md)
- add legend in targetpct rmarkdown-template
- add unit testing (testthat)

## explore 0.6.2 (2020-10-14)

CRAN release: 2020-10-14

- Bugfix [`explore_bar()`](../reference/explore_bar.md): `NA` in plot
- [`explore_count()`](../reference/explore_count.md): convert target
  into factor
- [`explore_count()`](../reference/explore_count.md): add default title
  (cat name)
- [`explore_count()`](../reference/explore_count.md): add parameter
  numeric, max_cat, max_target_cat
- [`explain_tree()`](../reference/explain_tree.md): convert character
  variables into factors (count data)
- [`explain_tree()`](../reference/explain_tree.md): parameter out
  (“plot” \| “model”)
- [`explain_logreg()`](../reference/explain_logreg.md): parameter out
  (“tibble” \| “model”)
- `vignette("explore_titanic")`: change to tibble
- `vignette("explore_mtcars")`: add explanations
- change theme_minimal to theme_light
- dwh_fastload(): add parameters overwrite and append
- update README.md

## explore 0.6.1 (2020-09-04)

CRAN release: 2020-09-04

- Fix Github URL
- new Vignette `vignette("explore_penguins")`
- new Vignette `vignette("explore_titanic")` (count data)
- [`explore_count()`](../reference/explore_count.md): plot count()
  output
- add default parameter `n` for count data:
  [`explore()`](../reference/explore.md),
  [`explore_all()`](../reference/explore_all.md),
  [`explore_tbl()`](../reference/explore_tbl.md),
  [`explain_tree()`](../reference/explain_tree.md),
  [`report()`](../reference/report.md),
  [`describe()`](../reference/describe.md),
  [`describe_cat()`](../reference/describe_cat.md),
  [`describe_num()`](../reference/describe_num.md),
  [`describe_tbl()`](../reference/describe_tbl.md),
  [`total_fig_height()`](../reference/total_fig_height.md)
- `explore_tree()`: default value for minsplit = 10% of obs
- [`explore_cor()`](../reference/explore_cor.md): use `geom_point()` for
  small datasets
- [`explore_shiny()`](../reference/explore_shiny.md): use
  [`browseURL()`](https://rdrr.io/r/utils/browseURL.html) with parameter
  `browser=NULL`
- [`describe_tbl()`](../reference/describe_tbl.md): add observations
  containing `NA`
- [`guess_cat_num()`](../reference/guess_cat_num.md): parameter
  description (optional)
- [`count_pct()`](../reference/count_pct.md): no renaming of variables.

## explore 0.5.5 (2020-04-06)

CRAN release: 2020-04-06

Maintenance update:

- fix breaking changes tibble 3.0.0

## explore 0.5.4 (2020-02-09)

CRAN release: 2020-02-09

Maintenance update:

- fix param `...` in description (PR#16223, see
  <https://bugs.r-project.org/show_bug.cgi?id=16223>)

## explore 0.5.3 (2020-01-17)

CRAN release: 2020-01-17

- [`explore_bar()`](../reference/explore_bar.md): add parameter numeric
- [`describe_all()`](../reference/describe_all.md) returns a tibble
- [`describe_all()`](../reference/describe_all.md): column ‘variable’ is
  character (not factor)
- [`report()`](../reference/report.md) split = TRUE as default
- add [`rescale01()`](../reference/rescale01.md)
- add parameter `rescale01` to
  [`clean_var()`](../reference/clean_var.md)
- add function [`count_pct()`](../reference/count_pct.md)
- add `out='tibble'` to [`describe_cat()`](../reference/describe_cat.md)
- add function
  [`explore_targetpct()`](../reference/explore_targetpct.md)

## explore 0.5.2 (2019-11-22)

CRAN release: 2019-11-22

- split source-code file into multiple files
- [`format_num_auto()`](../reference/format_num_auto.md) without
  brackets
- treat Date variables as cat
- [`report()`](../reference/report.md) fix automatic file extension
  .html
- add [`simplify_text()`](../reference/simplify_text.md)
- add parameter `simplify_text` to
  [`clean_var()`](../reference/clean_var.md)
- fix link in README.md

## explore 0.5.1 (2019-10-08)

CRAN release: 2019-10-08

Prepare for new dplyr 0.8.4
([\#2](https://github.com/rolkra/explore/issues/2),
[@romainfrancois](https://github.com/romainfrancois))

### Bug Fixes

- prepare [`explore_tbl()`](../reference/explore_tbl.md) for dplyr 0.8.4
- [`describe_num()`](../reference/describe_num.md) with default digits=6
- [`describe_cat()`](../reference/describe_cat.md) bugfix variable with
  all NA
- [`describe_all()`](../reference/describe_all.md) bugfix variable with
  all NA
- [`explain_tree()`](../reference/explain_tree.md) bugfix dataframe with
  0 rows
- improve speed [`describe()`](../reference/describe.md) text output
  (RMarkdown)
- [`explore()`](../reference/explore.md) now checks if data is a
  data.frame

## explore 0.5.0 (2019-09-19)

CRAN release: 2019-09-19

Interactive data exploration now accept categorical and numerical
targets (next to a binary target).

- [`explain_tree()`](../reference/explain_tree.md): target can be
  bin/num/cat
- [`explain_tree()`](../reference/explain_tree.md): add parameter
  max_target_cat
- [`explore_shiny()`](../reference/explore_shiny.md): target can be
  bin/num/cat
- add function [`format_num_auto()`](../reference/format_num_auto.md)
- [`total_fig_height()`](../reference/total_fig_height.md) replaces the
  now deprecated [`get_nrow()`](../reference/get_nrow.md).
- add parameter title to [`explore_cor()`](../reference/explore_cor.md)
- add support for POSIXct in [`describe()`](../reference/describe.md)
- improved handling of dataframes with no observations
- add parameter `title` to
  [`explore_density()`](../reference/explore_density.md)
- add parameter `nvar` to
  [`total_fig_height()`](../reference/total_fig_height.md)
- update README.md
- update Vignettes
- add NEWS.md
- add hex sticker

## explore 0.4.4 (2019-08-27)

CRAN release: 2019-08-27

Many functions now accept categorical and numerical targets (next to a
binary target). If you want to force which geom is used for
visualisation, you can use explore_bar() and
[`explore_density()`](../reference/explore_density.md). New function
[`explore_tbl()`](../reference/explore_tbl.md) to visualise a
dataframe/table (type of variables, number of NA, …)

- add function [`explore_bar()`](../reference/explore_bar.md)
- [`explore_density()`](../reference/explore_density.md) now using
  correct tidy eval, target cat \> 2 possible
- [`target_explore_cat()`](../reference/target_explore_cat.md) now using
  correct tidy eval
- [`target_explore_num()`](../reference/target_explore_num.md) now using
  correct tidy eval
- `add plot_var_info()` - plots a info-text to a variable as ggplot obj.
- [`plot_var_info()`](../reference/plot_var_info.md) used in
  explore/explore_all if
- [`plot_var_info()`](../reference/plot_var_info.md) used if explore
  empty data
- add parameter `max_cat` in
  [`explore_bar()`](../reference/explore_bar.md),
  [`explore_density()`](../reference/explore_density.md) and
  [`explain_tree()`](../reference/explain_tree.md)
- add [`explore_tbl()`](../reference/explore_tbl.md)
- drop `explore_cat()` & `explore_num()`
- rename template_report_target_den.html \> \_split.html
- intelligent placing of labels in plots
- add info window “generating report …” in
  [`explore_shiny()`](../reference/explore_shiny.md)
- `format_num()` -\> format_num_kMB(), format_num_space()
- [`format_target()`](../reference/format_target.md) -\> if numeric
  split 0/1 by mean
- [`report()`](../reference/report.md) -\> default .html file extension
- consistency showing NA info in explore-title
- parameter split: default = FALSE
- allow numeric (num) target in explore_all & report
- [`describe_tbl()`](../reference/describe_tbl.md) -\> fix target if not
  bin
- [`describe()`](../reference/describe.md): change out=“vector” to
  out=“list”

## explore 0.4.3 (2019-06-17)

CRAN release: 2019-06-17

- fix parameter in [`explore()`](../reference/explore.md): `auto_scale`,
  `na`
- fix number of `NA` in [`explore()`](../reference/explore.md) (move
  code before `auto_scale`)
- [`explore_density()`](../reference/explore_density.md) with target:
  drop plot title “propensity by”
- [`explore_shiny()`](../reference/explore_shiny.md): use output_dir /
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
- change naming “attribute” to “variable” (consistent)
