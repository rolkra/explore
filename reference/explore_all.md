# Explore all variables

Explore all variables of a dataset (create plots)

## Usage

``` r
explore_all(
  data,
  n,
  target,
  ncol = 2,
  targetpct,
  color = c("#ADD8E6", "#7BB8DA"),
  split = TRUE
)
```

## Arguments

- data:

  A dataset

- n:

  Weights variable (only for count data)

- target:

  Target variable (0/1 or FALSE/TRUE)

- ncol:

  Layout of plots (number of columns)

- targetpct:

  Plot variable as target% (FALSE/TRUE)

- color:

  Forece a default color (if possible)

- split:

  Split by target (TRUE\|FALSE)

## Value

Plot

## Examples

``` r
explore_all(iris)


iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
explore_all(iris, target = is_virginica)
```
