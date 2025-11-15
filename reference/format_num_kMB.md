# Format number as character string (kMB)

Formats a big number as k (1 000), M (1 000 000) or B (1 000 000 000)

## Usage

``` r
format_num_kMB(number = 0, digits = 1)
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
format_num_kMB(5500, digits = 2)
#> [1] "5.5k"
```
