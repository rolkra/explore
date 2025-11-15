# Add a random categorical variable to dataset

Add a random categorical variable to dataset

## Usage

``` r
add_var_random_cat(
  data,
  name = "random_cat",
  cat = LETTERS[1:6],
  prob,
  overwrite = TRUE,
  seed
)
```

## Arguments

- data:

  A dataset

- name:

  Name of new variable (as string)

- cat:

  Vector of categories

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
iris %>% add_var_random_cat() %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_cat
#> 1          5.1         3.5          1.4         0.2  setosa          E
#> 2          4.9         3.0          1.4         0.2  setosa          C
#> 3          4.7         3.2          1.3         0.2  setosa          E
#> 4          4.6         3.1          1.5         0.2  setosa          E
#> 5          5.0         3.6          1.4         0.2  setosa          F
#> 6          5.4         3.9          1.7         0.4  setosa          D
iris %>% add_var_random_cat(name = "my_cat") %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species my_cat
#> 1          5.1         3.5          1.4         0.2  setosa      B
#> 2          4.9         3.0          1.4         0.2  setosa      A
#> 3          4.7         3.2          1.3         0.2  setosa      F
#> 4          4.6         3.1          1.5         0.2  setosa      D
#> 5          5.0         3.6          1.4         0.2  setosa      F
#> 6          5.4         3.9          1.7         0.4  setosa      C
iris %>% add_var_random_cat(cat = c("Version A", "Version B")) %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_cat
#> 1          5.1         3.5          1.4         0.2  setosa  Version A
#> 2          4.9         3.0          1.4         0.2  setosa  Version B
#> 3          4.7         3.2          1.3         0.2  setosa  Version B
#> 4          4.6         3.1          1.5         0.2  setosa  Version A
#> 5          5.0         3.6          1.4         0.2  setosa  Version A
#> 6          5.4         3.9          1.7         0.4  setosa  Version B
iris %>% add_var_random_cat(cat = c(1,2,3,4,5)) %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_cat
#> 1          5.1         3.5          1.4         0.2  setosa          4
#> 2          4.9         3.0          1.4         0.2  setosa          5
#> 3          4.7         3.2          1.3         0.2  setosa          5
#> 4          4.6         3.1          1.5         0.2  setosa          1
#> 5          5.0         3.6          1.4         0.2  setosa          1
#> 6          5.4         3.9          1.7         0.4  setosa          2
```
