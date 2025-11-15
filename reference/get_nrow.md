# Get number of rows for a grid plot

This function is deprecated, please use
[`total_fig_height()`](total_fig_height.md) instead.

## Usage

``` r
get_nrow(varnames, exclude = 0, ncol = 2)
```

## Arguments

- varnames:

  List of variables to be plotted

- exclude:

  Number of variables that will be excluded from plot

- ncol:

  Number of columns (default = 2)

## Value

Number of rows

## Examples

``` r
if (FALSE) { # \dontrun{
get_nrow(names(iris), ncol = 2)
} # }
```
