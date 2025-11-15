# Balance target variable

Balances the target variable in your dataset using downsampling. Target
must be 0/1, FALSE/TRUE ore no/yes

## Usage

``` r
balance_target(data, target, min_prop = 0.1, seed)
```

## Arguments

- data:

  A dataset

- target:

  Target variable (0/1, TRUE/FALSE, yes/no)

- min_prop:

  Minimum proportion of one of the target categories

- seed:

  Seed for random number generator

## Value

Data

## Examples

``` r
iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
balanced <- balance_target(iris, target = is_versicolor, min_prop = 0.5)
describe(balanced, is_versicolor)
#> variable = is_versicolor
#> type     = double
#> na       = 0 of 100 (0%)
#> unique   = 2
#>        0 = 50 (50%)
#>        1 = 50 (50%)
```
