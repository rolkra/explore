# Generate a report of all variables

Generate a report of all variables If target is defined, the relation to
the target is reported

## Usage

``` r
report(data, n, target, targetpct, split, color, output_file, output_dir)
```

## Arguments

- data:

  A dataset

- n:

  Weights variable for count data

- target:

  Target variable (0/1 or `FALSE`/`TRUE`)

- targetpct:

  Plot variable as target% (`FALSE`/`TRUE`)

- split:

  Alternative to targetpct (split = !targetpct)

- color:

  User defined colors for plots (vector)

- output_file:

  Filename of the html report

- output_dir:

  Directory where to save the html report

## Examples

``` r
if (rmarkdown::pandoc_available("1.12.3")) {
 report(iris, output_dir = tempdir())
 }
#> ℹ Processing template: "template_report_variable.Rmd"
#> ✔ Report created at file:///tmp/RtmpFjhNLU/report_variable.html
```
