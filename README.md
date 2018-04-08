# explore

There are basically two ways to use the eplore package:

**Interactive data exploration**

Launch an interactive shiny-app that makes basic data exploration radically simple. You can inspect individual attributes, explore their relation to a binary target or create a fully automated report of all attributes.

**Functions for reducible data science**

There are basically two "verbs" to remember:
* explore - if you want to explore an attribute or the relationship between an attribute and a binary target
* describe - if you want to describe a dataset or an attribute (number of na, unique values, ...)
The output of these functions is a plot or a text.

## Installation

```r
# install from github
if (!require(remotes)) install.packages("remotes")
remotes::install_github("rolkra/explore")
```

## Interactive data exploration

Example how to use the explore package to explore the iris dataset

```r
# load package
library(explore)

# explore interactive
explore(iris)

# define a target (is Species setosa?)
iris$is_setosa <- ifelse(iris$Species == "setosa", 1, 0)

# explore interactive
explore(iris)
```

<img src="https://github.com/rolkra/explore/blob/master/explore_shiny_iris.png" alt="example interactive exploration" width="800">

## Functions for reducible data science

Example how to use the functions of the explore package to explore the iris dataset

```r
# load packages
library(explore)
library(magrittr)  # to use the pipe operator %>%

# explore Species
iris %>% explore(Species)

# explore Sepal.Length
iris %>% explore(Sepal.Length)

# define a target (is Species setosa?)
iris$is_setosa <- ifelse(iris$Species == "setosa", 1, 0)

# explore relationship between Sepal.Length and the target
iris %>% explore(Sepal.Length, target = is_setosa)

# describe dataset
describe(iris)

# describe Species
iris %>% describe(Species)
```
