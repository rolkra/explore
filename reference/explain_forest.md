# Explain a target using Random Forest.

Explain a target using Random Forest.

## Usage

``` r
explain_forest(data, target, ntree = 50, out = "plot", ...)
```

## Arguments

- data:

  A dataset

- target:

  Target variable (binary)

- ntree:

  Number of trees used for Random Forest

- out:

  Output of the function: "plot" \| "model" \| "importance" \| all"

- ...:

  Further arguments

## Value

Plot of importance (if out = "plot")

## Examples

``` r
data <- create_data_buy()
explain_forest(data, target = buy)
```
