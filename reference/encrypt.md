# encrypt text

encrypt text

## Usage

``` r
encrypt(text, codeletters = c(toupper(letters), letters, 0:9), shift = 18)
```

## Arguments

- text:

  A text (character)

- codeletters:

  A string of letters that are used for encryption

- shift:

  Number of elements shifted

## Value

Encrypted text

## Examples

``` r
encrypt("hello world")
#> [1] "zw336 E693v"
```
