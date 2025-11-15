# Clean variable

Clean variable (replace NA values, set min_val and max_val)

## Usage

``` r
clean_var(
  data,
  var,
  na = NA,
  min_val = NA,
  max_val = NA,
  max_cat = NA,
  rescale01 = FALSE,
  simplify_text = FALSE,
  name = NA
)
```

## Arguments

- data:

  A dataset

- var:

  Name of variable

- na:

  Value that replaces NA

- min_val:

  All values \< min_val are converted to min_val (var numeric or
  character)

- max_val:

  All values \> max_val are converted to max_val (var numeric or
  character)

- max_cat:

  Maximum number of different factor levels for categorical variable (if
  more, .OTHER is added)

- rescale01:

  IF TRUE, value is rescaled between 0 and 1 (var must be numeric)

- simplify_text:

  If TRUE, a character variable is simplified (trim, upper, ...)

- name:

  New name of variable (as string)

## Value

Dataset

## Examples

``` r
library(magrittr)
iris %>% clean_var(Sepal.Width, max_val = 3.5, name = "sepal_width") %>% head()
#>   Sepal.Length sepal_width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.5          1.4         0.2  setosa
#> 6          5.4         3.5          1.7         0.4  setosa
iris %>% clean_var(Sepal.Width, rescale01 = TRUE) %>% head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1   0.6250000          1.4         0.2  setosa
#> 2          4.9   0.4166667          1.4         0.2  setosa
#> 3          4.7   0.5000000          1.3         0.2  setosa
#> 4          4.6   0.4583333          1.5         0.2  setosa
#> 5          5.0   0.6666667          1.4         0.2  setosa
#> 6          5.4   0.7916667          1.7         0.4  setosa
```
