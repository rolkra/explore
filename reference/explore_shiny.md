# Explore dataset interactive

Launches a shiny app to explore a dataset

## Usage

``` r
explore_shiny(data, target, color = c("#ADD8E6", "#7BB8DA"))
```

## Arguments

- data:

  A dataset

- target:

  Target variable (0/1 or FALSE/TRUE)

- color:

  Color for plots (vector)

## Examples

``` r
# Only run examples in interactive R sessions
if (interactive())  {
   explore_shiny(iris)
}
```
