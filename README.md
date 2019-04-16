# explore

Simplifies Exploratory Data Analysis. There are two ways to use the eplore package:

**Interactive data exploration**

Launch an interactive shiny-app to explore data. You can inspect individual attributes, explore their relation to a binary target or create a fully automated report of all attributes with a few "mouseclicks".

**Functions for reproducible data science**

Use the functions behind the shiny app for "tidy EDA". There are basically three "verbs" to remember:
* explore - if you want to explore an attribute or the relationship between an attribute and a binary target. The output of these functions is a plot.
* describe - if you want to describe a dataset or an attribute (number of na, unique values, ...) The output of these functions is a text.
* explain - to create a simple model that explains a target

The explore package automatically checks if an attribute is categorial or numerical, chooses the best plot-type and handles outliers (autosacling).

## Installation

### github
```r
# install from github
if (!require(devtools)) install.packages("devtools")
devtools::install_github("rolkra/explore")
```
if you are behind a firewall, you may want to:

* Download and unzip the explore package
* Then install it with devtools::install_local

```r
# install local
if (!require(devtools)) install.packages("devtools")
devtools::install_local(path = <path of local package>, force = TRUE)
```

## Interactive data exploration

Example how to use the explore package to explore the iris dataset

```r
# load package
library(explore)

# define a target (is Species setosa?)
iris$is_setosa <- ifelse(iris$Species == "setosa", 1, 0)

# explore interactive
explore(iris)
```

### Explore attributes
<img src="https://github.com/rolkra/explore/blob/master/explore_shiny_iris.png" alt="example interactive exploration" width="800">

### Explain target
<img src="https://github.com/rolkra/explore/blob/master/explore_shiny_iris_tree.png" alt="example interactive exploration" width="800">

## Functions for reproducible data science

Example how to use the functions of the explore package to explore the iris dataset

```r
# load packages
library(explore)
library(magrittr)  # to use the pipe operator %>%

# use iris dataset
data(iris)

# explore Species
iris %>% explore(Species)

# explore Sepal.Length
iris %>% explore(Sepal.Length)

# define a target (is Species versicolor?)
iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)

# explore relationship between Sepal.Length and the target
iris %>% explore(Sepal.Length, target = is_versicolor)

# explore correlation between Sepal.Length and Petal.Length
iris %>% explore(Sepal.Length, Petal.Length)

# explore correlation between Sepal.Length, Petal.Length and a target
iris %>% explore(Sepal.Length, Petal.Length, target = is_versicolor)

# describe dataset
describe(iris)

# describe Species
iris %>% describe(Species)

# explain target using a decision tree
iris$Species <- NULL
iris %>% explain_tree(target = is_versicolor)
```
