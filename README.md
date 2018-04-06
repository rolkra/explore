# explore
R package for simple, elegant and fast data exploration

Using the explore packate to explore data is as simple as "explore(data)".
This launches an interactive shiny-app that makes basic data exploration radically simple. You can inspect individual attributes or create a fully automated report of all attributes.

```r
# explore iris dataset (interactive)
explore(iris)
```

If you want to use the functionality of the explore package for reproducable data science, there are basically two "verbs" to remember:
* explore - if you want to explore an attribute or the relationship between an attribute and a binary target
* describe - if you want to describe a dataset or an attribute (number of na, unique values, ...)

```r
# load package magrittr (to use the pipe operator %>%)
library(magrittr)

# explore Species
iris %>% explore(Species)

# explore Sepal.Length
iris %>% explore(Sepal.Length)

# define a target (is Species versicolor?)
iris$target_01 <- ifelse(iris$Species == "versicolor", 1, 0)

# explore relationship between Sepal.Length and the target
iris %>% explore(Sepal.Length, target = target_01)

# describe dataset
describe(iris)

# describe Species
iris %>% describe(Species)
```
