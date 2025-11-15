# Create data buy

Artificial data that can be used for unit-testing or teaching

## Usage

``` r
create_data_buy(
  obs = 1000,
  target_name = "buy",
  factorise_target = FALSE,
  target1_prob = 0.5,
  add_extreme = TRUE,
  flip_gender = FALSE,
  add_id = FALSE,
  seed = 123
)
```

## Arguments

- obs:

  Number of observations

- target_name:

  Variable name of target

- factorise_target:

  Should target variable be factorised? (from 0/1 to factor no/yes)?

- target1_prob:

  Probability that target = 1

- add_extreme:

  Add an observation with extreme values?

- flip_gender:

  Should Male/Female be flipped in data?

- add_id:

  Add an id-variable to data?

- seed:

  Seed for randomization

## Value

A dataset as tibble

## Details

Variables in dataset:

- id = Identifier

- period = Year & Month (YYYYMM)

- city_ind = Indicating if customer is residing in a city (1 = yes, 0 =
  no)

- female_ind = Gender of customer is female (1 = yes, 0 = no)

- fixedvoice_ind = Customer has a fixed voice product (1 = yes, 0 = no)

- fixeddata_ind = Customer has a fixed data product (1 = yes, 0 = no)

- fixedtv_ind = Customer has a fixed TV product (1 = yes, 0 = no)

- mobilevoice_ind = Customer has a mobile voice product (1 = yes, 0 =
  no)

- mobiledata_prd = Customer has a mobile data product (NO/MOBILE
  STICK/BUSINESS)

- bbi_speed_ind = Customer has a Broadband Internet (BBI) with extra
  speed

- bbi_usg_gb = Broadband Internet (BBI) usage in Gigabyte (GB) last
  month

- hh_single = Expected to be a Single Household (1 = yes, 0 = no)

Target in dataset:

- buy (may be renamed) = Did customer buy a new product in next month?
  (1 = yes, 0 = no)

## Examples

``` r
create_data_buy()
#> # A tibble: 1,000 × 13
#>    period   buy   age city_ind female_ind fixedvoice_ind fixeddata_ind
#>     <int> <int> <int>    <int>      <int>          <int>         <int>
#>  1 202012     1    39        1          0              0             1
#>  2 202012     0    57        0          0              0             1
#>  3 202012     1    55        0          1              0             1
#>  4 202012     0    66        0          0              0             1
#>  5 202012     0    71        0          1              0             1
#>  6 202012     1    44        1          0              0             1
#>  7 202012     0    64        0          0              0             1
#>  8 202012     0    51        1          0              0             1
#>  9 202012     0    70        1          0              0             1
#> 10 202012     1    44        1          1              0             1
#> # ℹ 990 more rows
#> # ℹ 6 more variables: fixedtv_ind <int>, mobilevoice_ind <int>,
#> #   mobiledata_prd <chr>, bbi_speed_ind <int>, bbi_usg_gb <int>,
#> #   hh_single <int>
```
