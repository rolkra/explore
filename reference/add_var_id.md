# Add a variable id at first column in dataset

Add a variable id at first column in dataset

## Usage

``` r
add_var_id(data, name = "id", overwrite = FALSE)
```

## Arguments

- data:

  A dataset

- name:

  Name of new variable (as string)

- overwrite:

  Can new id variable overwrite an existing variable in dataset?

## Value

Data set containing new id variable

## Examples

``` r
library(magrittr)
iris %>% add_var_id() %>% head()
#>   id Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1  1          5.1         3.5          1.4         0.2  setosa
#> 2  2          4.9         3.0          1.4         0.2  setosa
#> 3  3          4.7         3.2          1.3         0.2  setosa
#> 4  4          4.6         3.1          1.5         0.2  setosa
#> 5  5          5.0         3.6          1.4         0.2  setosa
#> 6  6          5.4         3.9          1.7         0.4  setosa
iris %>% add_var_id(name = "iris_nr") %>% head()
#>   iris_nr Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1       1          5.1         3.5          1.4         0.2  setosa
#> 2       2          4.9         3.0          1.4         0.2  setosa
#> 3       3          4.7         3.2          1.3         0.2  setosa
#> 4       4          4.6         3.1          1.5         0.2  setosa
#> 5       5          5.0         3.6          1.4         0.2  setosa
#> 6       6          5.4         3.9          1.7         0.4  setosa
```
