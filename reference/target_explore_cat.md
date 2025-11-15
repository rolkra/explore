# Explore categorical variable + target

Create a plot to explore relation between categorical variable and a
binary target

## Usage

``` r
target_explore_cat(
  data,
  var,
  target = "target_ind",
  min_val = NA,
  max_val = NA,
  flip = TRUE,
  num2char = TRUE,
  title = NA,
  auto_scale = TRUE,
  na = NA,
  max_cat = 25,
  color = c("#ECEFF1", "#CFD8DC", "#B0BEC5", "#90A4AE"),
  legend_position = "bottom"
)
```

## Arguments

- data:

  A dataset

- var:

  Categorical variable

- target:

  Target variable (0/1 or FALSE/TRUE)

- min_val:

  All values \< min_val are converted to min_val

- max_val:

  All values \> max_val are converted to max_val

- flip:

  Should plot be flipped? (change of x and y)

- num2char:

  If TRUE, numeric values in variable are converted into character

- title:

  Title of plot

- auto_scale:

  Not used, just for compatibility

- na:

  Value to replace NA

- max_cat:

  Maximum numbers of categories to be plotted

- color:

  Color vector (4 colors)

- legend_position:

  Position of legend ("right"\|"bottom"\|"non")

## Value

Plot object
