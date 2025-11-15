# Create data random

Random data that can be used for unit-testing or teaching

## Usage

``` r
create_data_random(
  obs = 1000,
  vars = 10,
  target_name = "target_ind",
  factorise_target = FALSE,
  target1_prob = 0.5,
  add_id = TRUE,
  seed = 123
)
```

## Arguments

- obs:

  Number of observations

- vars:

  Number of variables

- target_name:

  Variable name of target

- factorise_target:

  Should target variable be factorised? (from 0/1 to facotr no/yes)?

- target1_prob:

  Probability that target = 1

- add_id:

  Add an id-variable to data?

- seed:

  Seed for randomization

## Value

A dataset as tibble

## Details

Variables in dataset:

- id = Identifier

- var_X = variable containing values between 0 and 100

Target in dataset:

- target_ind (may be renamed) = random values (1 = yes, 0 = no)

## Examples

``` r
create_data_random(obs = 100, vars = 5)
#> # A tibble: 100 × 7
#>       id target_ind var_1 var_2 var_3 var_4 var_5
#>    <int>      <int> <int> <int> <int> <int> <int>
#>  1     1          1    60    24    78    99    35
#>  2     2          0    33    96     1    14    37
#>  3     3          1    49    60    78    91    29
#>  4     4          0    95    52    73    58     8
#>  5     5          0    48    40    63    40    37
#>  6     6          1    89    88    48    45    18
#>  7     7          0    91    36    16    71    54
#>  8     8          0    61    29     1     8    50
#>  9     9          0    41    17    45    34    95
#> 10    10          1    15    17    49    68    34
#> # ℹ 90 more rows
```
