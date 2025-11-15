# Put variables into "buckets" to create a set of plots instead one large plot

Put variables into "buckets" to create a set of plots instead one large
plot

## Usage

``` r
get_var_buckets(data, bucket_size = 100, var_name_target = NA, var_name_n = NA)
```

## Arguments

- data:

  A dataset

- bucket_size:

  Maximum number of variables in one bucket

- var_name_target:

  Name of the target variable (if defined)

- var_name_n:

  Name of the weight (n) variable (if defined)

## Value

Buckets as a list

## Examples

``` r
get_var_buckets(iris)
#> [[1]]
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"     
#> 
get_var_buckets(iris, bucket_size = 2)
#> [[1]]
#> [1] "Sepal.Length" "Sepal.Width" 
#> 
#> [[2]]
#> [1] "Petal.Length" "Petal.Width" 
#> 
#> [[3]]
#> [1] "Species"
#> 
get_var_buckets(iris, bucket_size = 2, var_name_target = "Species")
#> [[1]]
#> [1] "Sepal.Length" "Sepal.Width"  "Species"     
#> 
#> [[2]]
#> [1] "Petal.Length" "Petal.Width"  "Species"     
#> 
```
