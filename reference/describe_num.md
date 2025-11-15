# Describe numerical variable

Describe numerical variable

## Usage

``` r
describe_num(data, var, n, out = "text", margin = 0)
```

## Arguments

- data:

  A dataset

- var:

  Variable or variable name

- n:

  Weights variable for count-data

- out:

  Output format ("text"\|"list")

- margin:

  Left margin for text output (number of spaces)

## Value

Description as text or list

## Examples

``` r
describe_num(iris, Sepal.Length)
#> variable = Sepal.Length
#> type     = double
#> na       = 0 of 150 (0%)
#> unique   = 35
#> min|max  = 4.3 | 7.9
#> q05|q95  = 4.6 | 7.255
#> q25|q75  = 5.1 | 6.4
#> median   = 5.8
#> mean     = 5.843333
```
