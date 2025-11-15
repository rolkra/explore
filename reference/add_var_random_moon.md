# Add a random moon variable to dataset

Add a random moon variable to dataset

## Usage

``` r
add_var_random_moon(data, name = "random_moon", overwrite = TRUE, seed)
```

## Arguments

- data:

  A dataset

- name:

  Name of new variable (as string)

- overwrite:

  Can new random variable overwrite an existing variable in dataset?

- seed:

  Seed for random number generation (integer)

## Value

Dataset containing new random variable

## Examples

``` r
library(magrittr)
iris %>% add_var_random_moon() %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_moon
#> 1          5.1         3.5          1.4         0.2  setosa    Full (O)
#> 2          4.9         3.0          1.4         0.2  setosa  Waning (-)
#> 3          4.7         3.2          1.3         0.2  setosa  Waning (-)
#> 4          4.6         3.1          1.5         0.2  setosa    Full (O)
#> 5          5.0         3.6          1.4         0.2  setosa  Waxing (+)
#> 6          5.4         3.9          1.7         0.4  setosa  Waxing (+)
```
