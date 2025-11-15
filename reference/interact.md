# Make a explore-plot interactive

Make a explore-plot interactive

## Usage

``` r
interact(obj, lower_title = TRUE, hide_geom_text = TRUE)
```

## Arguments

- obj:

  A object (e.g. ggplot2-object)

- lower_title:

  Lowering the title in ggplot2-object(`FALSE`/`TRUE`)

- hide_geom_text:

  Hiding geom_text in ggplot2-object (`FALSE`/`TRUE`)

## Value

Plot object

## Examples

``` r
library(dplyr)
if (interactive())  {
   iris %>% explore(Sepal.Length, target = Species) %>% interact()
}
```
