# Use the wordle data set

This data set contains the result of a real wordle challange (in german
language) between tow players. Wordle is a game where a player guesses a
five-letter word in six tries. The variable "try" reflects the success
of player A and B. Other variables like "noun", "aeiou", "unique",
"common" and "rare" reflect the properties of the word.

## Usage

``` r
use_data_wordle()
```

## Value

Dataset

## Examples

``` r
use_data_wordle()
#> # A tibble: 364 × 10
#>    round word  language  noun player     try aeiou unique common  rare
#>    <int> <chr> <chr>    <int> <chr>    <int> <int>  <int>  <int> <int>
#>  1     1 infam German       0 Player A     4     2      5      3     1
#>  2     2 notiz German       1 Player A     2     2      5      4     1
#>  3     3 zille German       1 Player A     4     2      4      4     1
#>  4     4 stoer German       1 Player A     2     2      5      5     0
#>  5     5 wiege German       1 Player A     5     3      4      3     1
#>  6     6 seife German       1 Player A     4     3      4      4     1
#>  7     7 wanne German       1 Player A     4     2      4      4     1
#>  8     8 trick German       1 Player A     4     2      5      4     1
#>  9     9 suite German       1 Player A     4     3      5      5     0
#> 10    10 macho German       1 Player A     4     3      5      3     1
#> # ℹ 354 more rows
```
