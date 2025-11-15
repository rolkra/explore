# Explore categorical variable using bar charts

Create a barplot to explore a categorical variable. If a target is
selected, the barplot is created for all levels of the target.

## Usage

``` r
explore_bar(
  data,
  var,
  target,
  flip = NA,
  title = "",
  numeric = NA,
  max_cat = 30,
  max_target_cat = 5,
  color = c("#ADD8E6", "#7BB8DA"),
  legend_position = "right",
  label,
  label_size = 2.7,
  ...
)
```

## Arguments

- data:

  A dataset

- var:

  variable

- target:

  target (can have more than 2 levels)

- flip:

  Should plot be flipped? (change of x and y)

- title:

  Title of the plot (if empty var name)

- numeric:

  Display variable as numeric (not category)

- max_cat:

  Maximum number of categories to be plotted

- max_target_cat:

  Maximum number of categories to be plotted for target (except NA)

- color:

  Color for bar

- legend_position:

  Position of the legend ("bottom"\|"top"\|"none")

- label:

  Show labels? (if empty, automatic)

- label_size:

  Size of labels

- ...:

  Further arguments

## Value

Plot object (bar chart)
