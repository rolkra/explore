# Get predefined colors

Get predefined colors

## Usage

``` r
get_color(name, fill = FALSE, fill_color = "#DDDDDD", fill_n = 10)
```

## Arguments

- name:

  Name of color/color-vector

- fill:

  Fill color vector?

- fill_color:

  Color to use to fill color vector

- fill_n:

  Number of color codes to return

## Value

Vector of color-codes

## Examples

``` r
get_color("mario")
#>      mario      luigi      peach       toad     bowser donkeykong 
#>  "#e0102f"  "#08a936"  "#f096be"  "#17419a"  "#f8be10"  "#742607" 

get_color("mario")
#>      mario      luigi      peach       toad     bowser donkeykong 
#>  "#e0102f"  "#08a936"  "#f096be"  "#17419a"  "#f8be10"  "#742607" 
show_color(get_color("mario"))

show_color(get_color("mario", fill = TRUE, fill_n = 10))


col <- get_color("mario")
explore(iris, Sepal.Length, target = Species,
  color = col)

explore(iris, Sepal.Length, target = Species,
  color = c(col["peach"], col["bowser"], col["donkeykong"]))
```
