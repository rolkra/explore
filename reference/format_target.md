# Format target

Formats a target as a 0/1 variable. If target is numeric, 1 = above
average.

## Usage

``` r
format_target(target)
```

## Arguments

- target:

  Variable as vector

## Value

Formated target

## Examples

``` r
iris$is_virginica <- ifelse(iris$Species == "virginica", "yes", "no")
iris$target <- format_target(iris$is_virginica)
table(iris$target)
#> 
#>   0   1 
#> 100  50 
```
