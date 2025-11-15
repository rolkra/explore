# Format number as character string (space as big.mark)

Formats a big number using space as big.mark (1000 = 1 000)

## Usage

``` r
format_num_space(number = 0, digits = 1)
```

## Arguments

- number:

  A number (integer or real)

- digits:

  Number of digits

## Value

Formatted number as text

## Examples

``` r
format_num_space(5500, digits = 2)
#> [1] "5 500"
```
