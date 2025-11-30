# Explain a target using a simple decision tree (classification or regression)

Explain a target using a simple decision tree (classification or
regression)

## Usage

``` r
explain_tree(
  data,
  target,
  n,
  max_cat = 10,
  max_target_cat = 5,
  maxdepth = 3,
  minsplit = 20,
  cp = 0,
  weights = NA,
  size = 0.7,
  out = "plot",
  ...
)
```

## Arguments

- data:

  A dataset

- target:

  Target variable

- n:

  weights variable (for count data)

- max_cat:

  Drop categorical variables with higher number of levels

- max_target_cat:

  Maximum number of categories to be plotted for target (except NA)

- maxdepth:

  Set the maximum depth of any node of the final tree, with the root
  node counted as depth 0. Maximum value 30 (as node numbers have to be
  stored in 32-bit signed integer types).

- minsplit:

  the minimum number of observations that must exist in a node in order
  for a split to be attempted.

- cp:

  complexity parameter. Any split that does not decrease the overall
  lack of fit by a factor of `cp` is not attempted. For instance, with
  `anova` splitting, this means that the overall R-squared must increase
  by `cp` at each step. The main role of this parameter is to save
  computing time by pruning off splits that are obviously not
  worthwhile. Essentially,the user informs the program that any split
  which does not improve the fit by `cp` will likely be pruned off by
  cross-validation, and that hence the program need not pursue it.

- weights:

  optional case weights.

- size:

  Text size of plot

- out:

  Output of function: "plot" \| "model"

- ...:

  Further arguments

## Value

Plot or additional the model (if out = "model")

## Examples

``` r
data <- iris
data$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
data$Species <- NULL
explain_tree(data, target = is_versicolor)
```
