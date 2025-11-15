# Use the titanic data set

This data set comes with base R. Survival of passengers on the Titanic.

## Usage

``` r
use_data_titanic(count = FALSE)
```

## Arguments

- count:

  use count data

## Value

Dataset

## Examples

``` r
use_data_titanic(count = TRUE)
#> # A tibble: 32 × 5
#>    Class Sex    Age   Survived     n
#>    <chr> <chr>  <chr> <chr>    <dbl>
#>  1 1st   Male   Child No           0
#>  2 2nd   Male   Child No           0
#>  3 3rd   Male   Child No          35
#>  4 Crew  Male   Child No           0
#>  5 1st   Female Child No           0
#>  6 2nd   Female Child No           0
#>  7 3rd   Female Child No          17
#>  8 Crew  Female Child No           0
#>  9 1st   Male   Adult No         118
#> 10 2nd   Male   Adult No         154
#> # ℹ 22 more rows
use_data_titanic(count = FALSE)
#> # A tibble: 2,201 × 4
#>    Class Sex   Age   Survived
#>    <chr> <chr> <chr> <chr>   
#>  1 3rd   Male  Child No      
#>  2 3rd   Male  Child No      
#>  3 3rd   Male  Child No      
#>  4 3rd   Male  Child No      
#>  5 3rd   Male  Child No      
#>  6 3rd   Male  Child No      
#>  7 3rd   Male  Child No      
#>  8 3rd   Male  Child No      
#>  9 3rd   Male  Child No      
#> 10 3rd   Male  Child No      
#> # ℹ 2,191 more rows
```
