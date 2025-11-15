# Add a random integer variable to dataset

Add a random integer variable to dataset

## Usage

``` r
add_var_random_int(
  data,
  name = "random_int",
  min_val = 1,
  max_val = 10,
  overwrite = TRUE,
  seed
)
```

## Arguments

- data:

  A dataset

- name:

  Name of new variable (as string)

- min_val:

  Minimum random integers

- max_val:

  Maximum random integers

- overwrite:

  Can new random variable overwrite an existing variable in dataset?

- seed:

  Seed for random number generation (integer)

## Value

Dataset containing new random variable

## Examples

``` r
library(magrittr)
iris %>% add_var_random_int() %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_int
#> 1          5.1         3.5          1.4         0.2  setosa          5
#> 2          4.9         3.0          1.4         0.2  setosa          4
#> 3          4.7         3.2          1.3         0.2  setosa         10
#> 4          4.6         3.1          1.5         0.2  setosa          9
#> 5          5.0         3.6          1.4         0.2  setosa          8
#> 6          5.4         3.9          1.7         0.4  setosa          6
iris %>% add_var_random_int(name = "random_var") %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_var
#> 1          5.1         3.5          1.4         0.2  setosa          5
#> 2          4.9         3.0          1.4         0.2  setosa          3
#> 3          4.7         3.2          1.3         0.2  setosa          3
#> 4          4.6         3.1          1.5         0.2  setosa          6
#> 5          5.0         3.6          1.4         0.2  setosa          1
#> 6          5.4         3.9          1.7         0.4  setosa          3
iris %>% add_var_random_int(min_val = 1, max_val = 10) %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_int
#> 1          5.1         3.5          1.4         0.2  setosa         10
#> 2          4.9         3.0          1.4         0.2  setosa          6
#> 3          4.7         3.2          1.3         0.2  setosa          1
#> 4          4.6         3.1          1.5         0.2  setosa          8
#> 5          5.0         3.6          1.4         0.2  setosa          7
#> 6          5.4         3.9          1.7         0.4  setosa          9
```
