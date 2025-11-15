# Explain a binary target using a logistic regression (glm). Model chosen by AIC in a Stepwise Algorithm (`MASS::stepAIC()`).

Explain a binary target using a logistic regression (glm). Model chosen
by AIC in a Stepwise Algorithm
([`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html)).

## Usage

``` r
explain_logreg(data, target, out = "tibble", ...)
```

## Arguments

- data:

  A dataset

- target:

  Target variable (binary)

- out:

  Output of the function: "tibble" \| "model"

- ...:

  Further arguments

## Value

Dataset with results (term, estimate, std.error, z.value, p.value)

## Examples

``` r
data <- iris
data$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
data$Species <- NULL
explain_logreg(data, target = is_versicolor)
#> # A tibble: 4 × 5
#>   term         estimate std.error statistic    p.value
#>   <chr>           <dbl>     <dbl>     <dbl>      <dbl>
#> 1 (Intercept)      6.95     2.23       3.12 0.00179   
#> 2 Sepal.Width     -2.96     0.667     -4.43 0.00000926
#> 3 Petal.Length     1.13     0.462      2.44 0.0148    
#> 4 Petal.Width     -2.61     1.08      -2.42 0.0156    
```
