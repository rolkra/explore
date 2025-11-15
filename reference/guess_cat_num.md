# Return if variable is categorical or numerical

Guess if variable is categorical or numerical based on name, type and
values of variable

## Usage

``` r
guess_cat_num(var, descr)
```

## Arguments

- var:

  A vector (dataframe column)

- descr:

  A description of the variable (optional)

## Value

"cat" (categorical), "num" (numerical) or "oth" (other)

## Examples

``` r
guess_cat_num(iris$Species)
#> [1] "cat"
```
