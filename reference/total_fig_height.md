# Get fig.height for RMarkdown-junk using explore_all()

Get fig.height for RMarkdown-junk using explore_all()

## Usage

``` r
total_fig_height(
  data,
  var_name_n,
  var_name_target,
  nvar = NA,
  ncol = 2,
  size = 3
)
```

## Arguments

- data:

  A dataset

- var_name_n:

  Weights variable for count data? (TRUE / MISSING)

- var_name_target:

  Target variable (TRUE / MISSING)

- nvar:

  Number of variables to plot

- ncol:

  Number of columns (default = 2)

- size:

  fig.height of 1 plot (default = 3)

## Value

Number of rows

## Examples

``` r
total_fig_height(iris)
#> [1] 9
total_fig_height(iris, var_name_target = "Species")
#> [1] 6
total_fig_height(nvar = 5)
#> [1] 9
```
