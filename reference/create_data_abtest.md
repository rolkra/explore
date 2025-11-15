# Create data of A/B testing

Data that can be used for unit-testing or teaching

## Usage

``` r
create_data_abtest(
  n_a = 100,
  n_b = 100,
  success_a = 10,
  success_b = 5,
  success_unit = "count",
  count = TRUE
)
```

## Arguments

- n_a:

  Total size of group A

- n_b:

  Total size of group B

- success_a:

  Success in group A

- success_b:

  Success in group B

- success_unit:

  Unit ("count"\|"percent")

- count:

  Create as count-data (FALSE\|TRUE)

## Value

A dataset as tibble

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
create_data_abtest() %>% abtest()

create_data_abtest(
  n_a = 100,
  n_b = 100,
  success_a = 20,
  success_b = 30,
  success_unit = "count"
) %>% abtest()
```
