# Calculate with periods (format yyyymm)

Calculate with periods (format yyyymm)

## Usage

``` r
yyyymm_calc(yyyymm, add_month = 0, add_year = 0, diff_to = NA)
```

## Arguments

- yyyymm:

  Input vector of periods (format yyyymm)

- add_month:

  How many months to add (can be negative too)

- add_year:

  How many years to add (can be negative too)

- diff_to:

  Difference between date and yyyymm (format yyyymm)

## Value

Vector of periods (format yyyymm) or number of months

## Examples

``` r
yyyymm_calc(202412, add_month = 1)
#> [1] 202501
yyyymm_calc(c(202411,202412,202501), add_month = -1, add_year = 1)
#> [1] 202510 202511 202512
yyyymm_calc(202410, diff_to = 202501)
#> [1] 3
yyyymm_calc(c(202411,202412,202501,202502), diff_to = 202501)
#> [1]  2  1  0 -1
```
