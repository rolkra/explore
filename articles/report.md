# Report variables

Automated Exploratory Analysis (auto EDA) using the
[`report()`](../reference/report.md) function of the explore package.

In this example we use synthetic data created by the
[`create_data_churn()`](../reference/create_data_churn.md)function of
the explore package.

``` r
library(dplyr)
library(explore)
data <- create_data_churn()
```

Then we generate the report.

``` r
data %>% report(output_dir = tempdir())
```

## Explore

### Describe Data

![](report_files/figure-html/unnamed-chunk-3-1.png)

    #> 1 000 (1k) observations with 9 variables
    #> 0 observations containing missings (NA)
    #> 0 variables containing missings (NA)
    #> 0 variables with no variance

    #> # A tibble: 9 × 8
    #>   variable   type     na na_pct unique   min  mean   max
    #>   <chr>      <chr> <int>  <dbl>  <int> <dbl> <dbl> <dbl>
    #> 1 price      dbl       0      0     26     4 19.1     29
    #> 2 type       chr       0      0      3    NA NA       NA
    #> 3 usage      dbl       0      0    160     0 54.7    150
    #> 4 shared     int       0      0      2     0  0.4      1
    #> 5 device     chr       0      0      3    NA NA       NA
    #> 6 newsletter int       0      0      2     0  0.5      1
    #> 7 language   chr       0      0      4    NA NA       NA
    #> 8 duration   int       0      0    101     0 49.6    100
    #> 9 churn      dbl       0      0      2     0  0.35     1

### Explore Variables

![](report_files/figure-html/unnamed-chunk-7-1.png)
