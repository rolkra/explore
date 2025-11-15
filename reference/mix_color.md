# Mix colors

Mix colors

## Usage

``` r
mix_color(color1, color2 = NA, n = 5)
```

## Arguments

- color1:

  Color 1

- color2:

  Color 2

- n:

  Number of different colors that should be generated

## Value

Vector of color-codes

## Examples

``` r
mix_color("blue", n = 10)
#>  [1] "#D0D0FF" "#A2A2FF" "#7373FF" "#4545FF" "#1717FF" "#0000E7" "#0000B9"
#>  [8] "#00008B" "#00005C" "#00002E"
mix_color("gold", "red", n = 4)
#> [1] "#FFD700" "#FF8F00" "#FF4700" "#FF0000"
```
