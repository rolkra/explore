# explore

[![CRAN Version](http://www.r-pkg.org/badges/version/explore)](https://cran.r-project.org/package=explore)
[![Downloads](http://cranlogs.r-pkg.org/badges/explore)](https://cran.r-project.org/package=explore)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/explore)](https://cran.r-project.org/package=explore)

Simplifies Exploratory Data Analysis. 

## Why this package?

* **Fast success** for new R user. It is said that R has a steep learning curve, especially if you come from a GUI for your statistical analysis. Instead of learning a lot of R syntax before you can explore data, the explore package enables you to have instant success. You can start with just one function - explore()

* **Faster insights with less code** for experienced R user. Exploring a fresh new dataset is exciting. Instead of searching for syntax at Stackoverflow, use all your attention searching for interesting patterns in your data using just a handful of easy to remember functions.

## How to use it

There are three ways to use the package:

**Interactive data exploration** (univariat, bivariat, multivariat) limited to a binary target.

Generate an **Automated Report** with one line of code. The target can be binary, categorical or numeric.

**Manual exploration** using a easy to remember set of tidy functions. There are basically four "verbs" to remember:

* **explore** - if you want to explore an attribute or the relationship between an attribute and a binary target. The output of these functions is a plot.
* **describe** - if you want to describe a dataset or an attribute (number of na, unique values, ...) The output of these functions is a text.
* **explain** - to create a simple model that explains a target
* **report** - to generate an automated report of all variables

The explore package automatically checks if an attribute is categorial or numerical, chooses the best plot-type and handles outliers (autosacling).

## Installation

### CRAN
```r
install.packages("explore")
```

To install the explore package on Debian / Ubuntu, you may need to install some additional dependencies first:

```
sudo apt install unixodbc unixodbc-dev
install.packages("odbc")
install.packages("explore")
```

### DEV version (github)
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

### Report of attributes

Create a report by clicking the "report all" button (if no target is defined)

<img src="https://github.com/rolkra/explore/blob/master/report_attributes.png" alt="example report attributes" width="400">

### Report of correlations with target
Create a report by clicking the "report all" button (if target is defined)

<img src="https://github.com/rolkra/explore/blob/master/report_target.png" alt="example report attributes" width="400">

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
