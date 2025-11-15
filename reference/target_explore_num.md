# Explore Nuberical variable + target

Create a plot to explore relation between numerical variable and a
binary target

## Usage

``` r
target_explore_num(
  data,
  var,
  target = "target_ind",
  min_val = NA,
  max_val = NA,
  bins = 10,
  flip = TRUE,
  title = NA,
  auto_scale = TRUE,
  na = NA,
  color = c("#ECEFF1", "#CFD8DC", "#B0BEC5", "#90A4AE"),
  legend_position = "bottom"
)
```

## Arguments

- data:

  A dataset

- var:

  Numerical variable

- target:

  Target variable (0/1 or FALSE/TRUE)

- min_val:

  All values \< min_val are converted to min_val

- max_val:

  All values \> max_val are converted to max_val

- bins:

  Nuber of bins

- flip:

  Should plot be flipped? (change of x and y)

- title:

  Title of plot

- auto_scale:

  Use 0.02 and 0.98 quantile for min_val and max_val (if min_val and
  max_val are not defined)

- na:

  Value to replace NA

- color:

  Color vector (4 colors)

- legend_position:

  Position of legend ("right"\|"bottom"\|"non")

## Value

Plot object
