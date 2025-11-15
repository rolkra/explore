# Return type of variable

Return value of typeof, except if variable contains hide, then return
"other"

## Usage

``` r
get_type(var)
```

## Arguments

- var:

  A vector (dataframe column)

## Value

Value of typeof or "other"

## Examples

``` r
get_type(iris$Species)
#> [1] "factor"
```
