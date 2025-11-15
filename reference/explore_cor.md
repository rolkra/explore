# Explore the correlation between two variables

Explore the correlation between two variables

## Usage

``` r
explore_cor(
  data,
  x,
  y,
  target,
  bins = 8,
  min_val = NA,
  max_val = NA,
  auto_scale = TRUE,
  title = NA,
  color = c("#ADD8E6", "#7BB8DA"),
  ...
)
```

## Arguments

- data:

  A dataset

- x:

  Variable on x axis

- y:

  Variable on y axis

- target:

  Target variable (categorical)

- bins:

  Number of bins

- min_val:

  All values \< min_val are converted to min_val

- max_val:

  All values \> max_val are converted to max_val

- auto_scale:

  Use 0.2 and 0.98 quantile for min_val and max_val (if min_val and
  max_val are not defined)

- title:

  Title of the plot

- color:

  Color of the plot

- ...:

  Further arguments

## Value

Plot

## Examples

``` r
explore_cor(iris, x = Sepal.Length, y = Sepal.Width)
```
