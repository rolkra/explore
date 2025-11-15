# A/B testing comparing two mean

A/B testing comparing two mean

## Usage

``` r
abtest_targetnum(data, expr, target, sign_level = 0.05, color = "grey")
```

## Arguments

- data:

  A dataset

- expr:

  Expression, that results in a FALSE/TRUE

- target:

  Target variable (must be numeric)

- sign_level:

  Significance Level (typical 0.01/0.05/0.10)

- color:

  fill color

## Value

Plot that shows if difference is significant

## Examples

``` r
data <- create_data_buy(obs = 100)
abtest(data, city_ind == 1, target = age)
```
