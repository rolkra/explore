# A/B testing interactive

Launches a shiny app to A/B test

## Usage

``` r
abtest_shiny(
  size_a = 100,
  size_b = 100,
  success_a = 10,
  success_b = 20,
  success_unit = "percent",
  sign_level = 0.05
)
```

## Arguments

- size_a:

  Size of Group A

- size_b:

  Size of Group B

- success_a:

  Success of Group A

- success_b:

  Success of Group B

- success_unit:

  "count" \| "percent"

- sign_level:

  Significance Level (typical 0.01/0.05/0.10)

## Examples

``` r
# Only run examples in interactive R sessions
if (interactive())  {
   abtest_shiny()
}
```
