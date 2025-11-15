# Weight target variable

Create weights for the target variable in your dataset so that are equal
weights for target = 0 and target = 1. Target must be 0/1, FALSE/TRUE
ore no/yes

## Usage

``` r
weight_target(data, target)
```

## Arguments

- data:

  A dataset

- target:

  Target variable (0/1, TRUE/FALSE, yes/no)

## Value

Weights for each observation (as a vector)

## Examples

``` r
iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
weights <- weight_target(iris, target = is_versicolor)
versicolor <- iris$is_versicolor
table(versicolor, weights)
#>           weights
#> versicolor   1   2
#>          0 100   0
#>          1   0  50
```
