# Add a random 0/1 variable to dataset

Add a random 0/1 variable to dataset

## Usage

``` r
add_var_random_01(
  data,
  name = "random_01",
  prob = c(0.5, 0.5),
  overwrite = TRUE,
  seed
)
```

## Arguments

- data:

  A dataset

- name:

  Name of new variable (as string)

- prob:

  Vector of probabilities

- overwrite:

  Can new random variable overwrite an existing variable in dataset?

- seed:

  Seed for random number generation (integer)

## Value

Dataset containing new random variable

## Examples

``` r
library(magrittr)
iris %>% add_var_random_01() %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_01
#> 1          5.1         3.5          1.4         0.2  setosa         0
#> 2          4.9         3.0          1.4         0.2  setosa         1
#> 3          4.7         3.2          1.3         0.2  setosa         0
#> 4          4.6         3.1          1.5         0.2  setosa         0
#> 5          5.0         3.6          1.4         0.2  setosa         0
#> 6          5.4         3.9          1.7         0.4  setosa         1
iris %>% add_var_random_01(name = "my_var") %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species my_var
#> 1          5.1         3.5          1.4         0.2  setosa      0
#> 2          4.9         3.0          1.4         0.2  setosa      0
#> 3          4.7         3.2          1.3         0.2  setosa      0
#> 4          4.6         3.1          1.5         0.2  setosa      1
#> 5          5.0         3.6          1.4         0.2  setosa      0
#> 6          5.4         3.9          1.7         0.4  setosa      1
```
