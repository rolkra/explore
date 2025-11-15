# A/B testing

A/B testing

## Usage

``` r
abtest(data, expr, n, target, sign_level = 0.05, color = "grey")
```

## Arguments

- data:

  A dataset. If no data is provided, a shiny app is launched

- expr:

  Logical expression, that return in a FALSE/TRUE

- n:

  A Variable for number of observations (count data)

- target:

  Target variable

- sign_level:

  Significance Level (typical 0.01/0.05/0.10)

- color:

  Fill color of bar/violin-plot

## Value

Plot that shows if difference is significant

## Examples

``` r
## Using chi2-test or t-test depending on target type
data <- create_data_buy(obs = 100)
abtest(data, female_ind == 1, target = buy)  # chi2 test

abtest(data, city_ind == 1, target = age)    # t test


## If small number of observations, Fisher's Exact test
## is used for a binary target (if <= 5 observations in a subgroup)
data <- create_data_buy(obs = 25, seed = 1)
abtest(data, female_ind == 1, target = buy)  # Fisher's Exact test
```
