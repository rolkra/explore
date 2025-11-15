# Explore variable + binary target (values 0/1)

Create a plot to explore relation between a variable and a binary target
as target percent. The target variable is choosen automatically if
possible (name starts with 'target')

## Usage

``` r
explore_targetpct(
  data,
  var,
  target = NULL,
  title = NA,
  min_val = NA,
  max_val = NA,
  auto_scale = TRUE,
  na = NA,
  flip = NA,
  ...
)
```

## Arguments

- data:

  A dataset

- var:

  Numerical variable

- target:

  Target variable (0/1 or FALSE/TRUE)

- title:

  Title of the plot

- min_val:

  All values \< min_val are converted to min_val

- max_val:

  All values \> max_val are converted to max_val

- auto_scale:

  Use 0.2 and 0.98 quantile for min_val and max_val (if min_val and
  max_val are not defined)

- na:

  Value to replace NA

- flip:

  Flip plot? (for categorical variables)

- ...:

  Further arguments

## Value

Plot object

## Examples

``` r
iris$target01 <- ifelse(iris$Species == "versicolor",1,0)
explore_targetpct(iris)
```
