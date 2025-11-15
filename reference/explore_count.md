# Explore count data (categories + frequency)

Create a plot to explore count data (categories + freuency) Variable
named 'n' is auto detected as Frequency

## Usage

``` r
explore_count(
  data,
  cat,
  n,
  target,
  pct = FALSE,
  split = TRUE,
  title = NA,
  numeric = FALSE,
  max_cat = 30,
  max_target_cat = 5,
  color = c("#ADD8E6", "#7BB8DA"),
  flip = NA
)
```

## Arguments

- data:

  A dataset (categories + frequency)

- cat:

  Numerical variable

- n:

  Number of observations (frequency)

- target:

  Target variable

- pct:

  Show as percent?

- split:

  Split by target (FALSE/TRUE)

- title:

  Title of the plot

- numeric:

  Display variable as numeric (not category)

- max_cat:

  Maximum number of categories to be plotted

- max_target_cat:

  Maximum number of categories to be plotted for target (except NA)

- color:

  Color for bar

- flip:

  Flip plot? (for categorical variables)

## Value

Plot object

## Examples

``` r
library(dplyr)
iris %>%
  count(Species) %>%
  explore_count(Species)
```
