# Predict target using a trained model.

Predict target using a trained model.

## Usage

``` r
predict_target(data, model, name = "prediction")
```

## Arguments

- data:

  A dataset (data.frame or tbl)

- model:

  A model created with `explain_*()` function

- name:

  Prefix of variable-name for prediction

## Value

data containing predicted probabilities for target values

## Examples

``` r
data_train <- create_data_buy(seed = 1)
data_test <- create_data_buy(seed = 2)
model <- explain_tree(data_train, target = buy, out = "model")

data <- predict_target(data = data_test, model = model)
describe(data)
#> # A tibble: 15 × 8
#>    variable        type     na na_pct unique       min      mean       max
#>    <chr>           <chr> <int>  <dbl>  <int>     <dbl>     <dbl>     <dbl>
#>  1 period          int       0      0      1 202012    202012    202012   
#>  2 buy             int       0      0      2      0         0.5       1   
#>  3 age             int       0      0     65     20        52.7      95   
#>  4 city_ind        int       0      0      2      0         0.5       1   
#>  5 female_ind      int       0      0      2      0         0.53      1   
#>  6 fixedvoice_ind  int       0      0      2      0         0.1       1   
#>  7 fixeddata_ind   int       0      0      1      1         1         1   
#>  8 fixedtv_ind     int       0      0      2      0         0.4       1   
#>  9 mobilevoice_ind int       0      0      2      0         0.59      1   
#> 10 mobiledata_prd  chr       0      0      3     NA        NA        NA   
#> 11 bbi_speed_ind   int       0      0      2      0         0.61      1   
#> 12 bbi_usg_gb      int       0      0     88      7       164.   100000   
#> 13 hh_single       int       0      0      2      0         0.33      1   
#> 14 prediction_0    dbl       0      0      6      0.1       0.5       0.88
#> 15 prediction_1    dbl       0      0      6      0.12      0.5       0.9 
```
