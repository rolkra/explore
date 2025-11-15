# Check vector for low variance

Check vector for low variance

## Usage

``` r
check_vec_low_variance(values, max_prop = 0.99)
```

## Arguments

- values:

  Vector of values

- max_prop:

  Maximum proportion of values without variance

## Value

TRUE/FALSE (low variance)

## Examples

``` r
if (FALSE) { # \dontrun{
values <- c(1, rep(0 ,1000))
check_vec_low_variance(values, max_prop = 0.9)
} # }
```
