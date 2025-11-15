# Simplifies a text string

A text string is converted into a simplified version by trimming,
converting to upper case, replacing german Umlaute, dropping special
characters like comma and semicolon and replacing multiple spaces with
one space.

## Usage

``` r
simplify_text(text)
```

## Arguments

- text:

  text string

## Value

text string

## Examples

``` r
simplify_text(" Hello  World !, ")
#> [1] "HELLO WORLD !"
```
