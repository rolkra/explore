# decrypt text

decrypt text

## Usage

``` r
decrypt(text, codeletters = c(toupper(letters), letters, 0:9), shift = 18)
```

## Arguments

- text:

  A text (character)

- codeletters:

  A string of letters that are used for decryption

- shift:

  Number of elements shifted

## Value

Decrypted text

## Examples

``` r
decrypt("zw336 E693v")
#> [1] "hello world"
```
