# Replace NA

Replace NA values of a variable in a dataframe

## Usage

``` r
replace_na_with(data, var_name, with)
```

## Arguments

- data:

  A dataframe

- var_name:

  Name of variable where NAs are replaced

- with:

  Value instead of NA

## Value

Updated dataframe

## Examples

``` r
data <- data.frame(nr = c(1,2,3,NA,NA))
replace_na_with(data, "nr", 0)
#> replace NA in variable nr with 0
#>   nr
#> 1  1
#> 2  2
#> 3  3
#> 4  0
#> 5  0
```
