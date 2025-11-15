# A/B testing comparing percent per group

A/B testing comparing percent per group

## Usage

``` r
abtest_targetpct(
  data,
  expr,
  n,
  target,
  sign_level = 0.05,
  group_label,
  ab_label = FALSE,
  color = "grey"
)
```

## Arguments

- data:

  A dataset

- expr:

  Expression, that results in a FALSE/TRUE

- n:

  A Variable for number of observations (count data)

- target:

  Target variable (must be 0/1 or FALSE/TRUE)

- sign_level:

  Significance Level (typical 0.01/0.05/0.10)

- group_label:

  Label of groups (default = expr)

- ab_label:

  Label Groups as A and B (default = FALSE)

- color:

  color of bar

## Value

Plot that shows if difference is significant

## Examples

``` r
data <- create_data_buy(obs = 100)
abtest(data, female_ind == 1, target = buy)

abtest(data, age >= 40, target = buy)
```
