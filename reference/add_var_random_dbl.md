# Add a random double variable to dataset

Add a random double variable to dataset

## Usage

``` r
add_var_random_dbl(
  data,
  name = "random_dbl",
  min_val = 0,
  max_val = 100,
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
iris %>% add_var_random_dbl() %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_dbl
#> 1          5.1         3.5          1.4         0.2  setosa   4.359784
#> 2          4.9         3.0          1.4         0.2  setosa  60.653479
#> 3          4.7         3.2          1.3         0.2  setosa  41.704910
#> 4          4.6         3.1          1.5         0.2  setosa   4.361902
#> 5          5.0         3.6          1.4         0.2  setosa  99.340774
#> 6          5.4         3.9          1.7         0.4  setosa  62.656104
iris %>% add_var_random_dbl(name = "random_var") %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_var
#> 1          5.1         3.5          1.4         0.2  setosa   91.58483
#> 2          4.9         3.0          1.4         0.2  setosa   56.90790
#> 3          4.7         3.2          1.3         0.2  setosa   63.78907
#> 4          4.6         3.1          1.5         0.2  setosa   41.60700
#> 5          5.0         3.6          1.4         0.2  setosa   79.66102
#> 6          5.4         3.9          1.7         0.4  setosa   86.22762
iris %>% add_var_random_dbl(min_val = 1, max_val = 10) %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_dbl
#> 1          5.1         3.5          1.4         0.2  setosa   7.099028
#> 2          4.9         3.0          1.4         0.2  setosa   8.516400
#> 3          4.7         3.2          1.3         0.2  setosa   9.426371
#> 4          4.6         3.1          1.5         0.2  setosa   1.526056
#> 5          5.0         3.6          1.4         0.2  setosa   7.012232
#> 6          5.4         3.9          1.7         0.4  setosa   4.472709
```
