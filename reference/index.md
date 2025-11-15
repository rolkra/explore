# Package index

## Explore

- [`explore()`](explore.md) : Explore a dataset or variable
- [`explore_all()`](explore_all.md) : Explore all variables
- [`explore_bar()`](explore_bar.md) : Explore categorical variable using
  bar charts
- [`explore_col()`](explore_col.md) : Explore data without aggregation
  (label + value)
- [`explore_cor()`](explore_cor.md) : Explore the correlation between
  two variables
- [`explore_count()`](explore_count.md) : Explore count data
  (categories + frequency)
- [`explore_density()`](explore_density.md) : Explore density of
  variable
- [`explore_shiny()`](explore_shiny.md) : Explore dataset interactive
- [`explore_targetpct()`](explore_targetpct.md) : Explore variable +
  binary target (values 0/1)
- [`explore_tbl()`](explore_tbl.md) : Explore table
- [`interact()`](interact.md) : Make a explore-plot interactive

## Describe

- [`describe()`](describe.md) : Describe a dataset or variable
- [`describe_all()`](describe_all.md) : Describe all variables of a
  dataset
- [`describe_cat()`](describe_cat.md) : Describe categorical variable
- [`describe_num()`](describe_num.md) : Describe numerical variable
- [`describe_tbl()`](describe_tbl.md) : Describe table

## Explain

- [`explain_forest()`](explain_forest.md) : Explain a target using
  Random Forest.

- [`explain_logreg()`](explain_logreg.md) :

  Explain a binary target using a logistic regression (glm). Model
  chosen by AIC in a Stepwise Algorithm
  ([`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html)).

- [`explain_tree()`](explain_tree.md) : Explain a target using a simple
  decision tree (classification or regression)

- [`explain_xgboost()`](explain_xgboost.md) : Explain a binary target
  using xgboost

- [`balance_target()`](balance_target.md) : Balance target variable

- [`weight_target()`](weight_target.md) : Weight target variable

- [`predict_target()`](predict_target.md) : Predict target using a
  trained model.

## Report

- [`report()`](report.md) : Generate a report of all variables
- [`get_var_buckets()`](get_var_buckets.md) : Put variables into
  "buckets" to create a set of plots instead one large plot
- [`total_fig_height()`](total_fig_height.md) : Get fig.height for
  RMarkdown-junk using explore_all()
- [`plot_legend_targetpct()`](plot_legend_targetpct.md) : Plots a legend
  that can be used for explore_all with a binary target
- [`create_notebook_explore()`](create_notebook_explore.md) : Generate a
  notebook

## A/B test

- [`abtest()`](abtest.md) : A/B testing
- [`abtest_shiny()`](abtest_shiny.md) : A/B testing interactive
- [`abtest_targetnum()`](abtest_targetnum.md) : A/B testing comparing
  two mean
- [`abtest_targetpct()`](abtest_targetpct.md) : A/B testing comparing
  percent per group

## Data

- [`add_var_id()`](add_var_id.md) : Add a variable id at first column in
  dataset
- [`add_var_random_01()`](add_var_random_01.md) : Add a random 0/1
  variable to dataset
- [`add_var_random_cat()`](add_var_random_cat.md) : Add a random
  categorical variable to dataset
- [`add_var_random_dbl()`](add_var_random_dbl.md) : Add a random double
  variable to dataset
- [`add_var_random_int()`](add_var_random_int.md) : Add a random integer
  variable to dataset
- [`add_var_random_moon()`](add_var_random_moon.md) : Add a random moon
  variable to dataset
- [`add_var_random_starsign()`](add_var_random_starsign.md) : Add a
  random starsign variable to dataset
- [`create_data_abtest()`](create_data_abtest.md) : Create data of A/B
  testing
- [`create_data_app()`](create_data_app.md) : Create data app
- [`create_data_buy()`](create_data_buy.md) : Create data buy
- [`create_data_churn()`](create_data_churn.md) : Create data churn
- [`create_data_empty()`](create_data_empty.md) : Create an empty
  dataset
- [`create_data_esoteric()`](create_data_esoteric.md) : Create data
  esoteric
- [`create_data_newsletter()`](create_data_newsletter.md) : Create data
  newsletter
- [`create_data_person()`](create_data_person.md) : Create data person
- [`create_data_random()`](create_data_random.md) : Create data random
- [`create_data_unfair()`](create_data_unfair.md) : Create data unfair
- [`use_data_beer()`](use_data_beer.md) : Use the beer data set
- [`use_data_diamonds()`](use_data_diamonds.md) : Use the diamonds data
  set
- [`use_data_iris()`](use_data_iris.md) : Use the iris flower data set
- [`use_data_mpg()`](use_data_mpg.md) : Use the mpg data set
- [`use_data_mtcars()`](use_data_mtcars.md) : Use the mtcars data set
- [`use_data_penguins()`](use_data_penguins.md) : Use the penguins data
  set
- [`use_data_starwars()`](use_data_starwars.md) : Use the starwars data
  set
- [`use_data_titanic()`](use_data_titanic.md) : Use the titanic data set
- [`use_data_wordle()`](use_data_wordle.md) : Use the wordle data set

## Clean / Drop

- [`clean_var()`](clean_var.md) : Clean variable
- [`drop_obs_if()`](drop_obs_if.md) : Drop all observations where
  expression is true
- [`drop_obs_with_na()`](drop_obs_with_na.md) : Drop all observations
  with NA-values
- [`drop_var_by_names()`](drop_var_by_names.md) : Drop variables by name
- [`drop_var_low_variance()`](drop_var_low_variance.md) : Drop all
  variables with low variance
- [`drop_var_no_variance()`](drop_var_no_variance.md) : Drop all
  variables with no variance
- [`drop_var_not_numeric()`](drop_var_not_numeric.md) : Drop all not
  numeric variables
- [`drop_var_with_na()`](drop_var_with_na.md) : Drop all variables with
  NA-values

## Tools

- [`count_pct()`](count_pct.md) : Adds percentage to dplyr::count()
- [`data_dict_md()`](data_dict_md.md) : Create a data dictionary
  Markdown file
- [`decrypt()`](decrypt.md) : decrypt text
- [`encrypt()`](encrypt.md) : encrypt text
- [`get_type()`](get_type.md) : Return type of variable
- [`guess_cat_num()`](guess_cat_num.md) : Return if variable is
  categorical or numerical
- [`plot_text()`](plot_text.md) : Plot a text
- [`replace_na_with()`](replace_na_with.md) : Replace NA
- [`rescale01()`](rescale01.md) : Rescales a numeric variable into
  values between 0 and 1
- [`simplify_text()`](simplify_text.md) : Simplifies a text string
- [`get_color()`](get_color.md) : Get predefined colors
- [`mix_color()`](mix_color.md) : Mix colors
- [`show_color()`](show_color.md) : Show color vector as ggplot
- [`yyyymm_calc()`](yyyymm_calc.md) : Calculate with periods (format
  yyyymm)
- [`format_num_auto()`](format_num_auto.md) : Format number as character
  string (auto)
- [`format_num_kMB()`](format_num_kMB.md) : Format number as character
  string (kMB)
- [`format_num_space()`](format_num_space.md) : Format number as
  character string (space as big.mark)
- [`format_target()`](format_target.md) : Format target
- [`format_type()`](format_type.md) : Format type description
- [`target_explore_cat()`](target_explore_cat.md) : Explore categorical
  variable + target
- [`target_explore_num()`](target_explore_num.md) : Explore Nuberical
  variable + target

## Internal

- [`log_info_if()`](log_info_if.md) : Log conditional
- [`plot_var_info()`](plot_var_info.md) : Plot a variable info
- [`check_vec_low_variance()`](check_vec_low_variance.md) : Check vector
  for low variance
- [`cut_vec_num_avg()`](cut_vec_num_avg.md) : Cut a variable
