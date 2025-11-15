# Describe categorical variable

Describe categorical variable

## Usage

``` r
describe_cat(data, var, n, max_cat = 10, out = "text", margin = 0)
```

## Arguments

- data:

  A dataset

- var:

  Variable or variable name

- n:

  Weights variable for count-data

- max_cat:

  Maximum number of categories displayed

- out:

  Output format ("text"\|"list"\|"tibble"\|"df")

- margin:

  Left margin for text output (number of spaces)

## Value

Description as text or list

## Examples

``` r
describe_cat(iris, Species)
#> variable = Species
#> type     = factor
#> na       = 0 of 150 (0%)
#> unique   = 3
#>  setosa     = 50 (33.3%)
#>  versicolor = 50 (33.3%)
#>  virginica  = 50 (33.3%)
```
