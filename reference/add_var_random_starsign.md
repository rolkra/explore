# Add a random starsign variable to dataset

Add a random starsign variable to dataset

## Usage

``` r
add_var_random_starsign(
  data,
  name = "random_starsign",
  lang = "en",
  overwrite = TRUE,
  seed
)
```

## Arguments

- data:

  A dataset

- name:

  Name of new variable (as string)

- lang:

  Language used for starsign (en = English, de = Deutsch, es = Espanol)

- overwrite:

  Can new random variable overwrite an existing variable in dataset?

- seed:

  Seed for random number generation (integer)

## Value

Dataset containing new random variable

## Examples

``` r
library(magrittr)
iris %>% add_var_random_starsign() %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_starsign
#> 1          5.1         3.5          1.4         0.2  setosa     Saggitarius
#> 2          4.9         3.0          1.4         0.2  setosa         Scorpio
#> 3          4.7         3.2          1.3         0.2  setosa           Libra
#> 4          4.6         3.1          1.5         0.2  setosa          Pisces
#> 5          5.0         3.6          1.4         0.2  setosa          Cancer
#> 6          5.4         3.9          1.7         0.4  setosa        Aquarius
iris %>% add_var_random_starsign(lang = "de") %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species random_starsign
#> 1          5.1         3.5          1.4         0.2  setosa           Loewe
#> 2          4.9         3.0          1.4         0.2  setosa        Schuetze
#> 3          4.7         3.2          1.3         0.2  setosa           Waage
#> 4          4.6         3.1          1.5         0.2  setosa        Skorpion
#> 5          5.0         3.6          1.4         0.2  setosa        Schuetze
#> 6          5.4         3.9          1.7         0.4  setosa        Zwilling
```
