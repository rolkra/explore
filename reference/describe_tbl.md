# Describe table

Describe table (e.g. number of rows and columns of dataset)

## Usage

``` r
describe_tbl(data, n, target, out = "text")
```

## Arguments

- data:

  A dataset

- n:

  Weights variable for count-data

- target:

  Target variable (binary)

- out:

  Output format ("text"\|"list")

## Value

Description as text or list

## Examples

``` r
describe_tbl(iris)
#> 150 observations with 5 variables
#> 0 observations containing missings (NA)
#> 0 variables containing missings (NA)
#> 0 variables with no variance

iris[1,1] <- NA
describe_tbl(iris)
#> 150 observations with 5 variables
#> 1 observations containing missings (NA)
#> 1 variables containing missings (NA)
#> 0 variables with no variance
```
