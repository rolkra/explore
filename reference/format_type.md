# Format type description

Format type description of variable to 3 letters
(int\|dbl\|lgl\|chr\|dat)

## Usage

``` r
format_type(type)
```

## Arguments

- type:

  Type description ("integer", "double", "logical", character", "date")

## Value

Formatted type description (int\|dbl\|lgl\|chr\|dat)

## Examples

``` r
format_type(typeof(iris$Species))
#> [1] "int"
```
