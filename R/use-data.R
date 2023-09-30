#' Use the iris flower data set
#'
#' This data set comes with base R. The data set gives the measurements
#' in centimeters of the variables sepal length and width and petal length and
#' width, respectively, for 50 flowers from each of 3 species of iris.
#' The species are Iris setosa, versicolor, and virginica.
#' @return Dataset as tibble
#' @examples
#' use_data_iris()
#' @export

use_data_iris <- function() {
  file <- system.file("extdata", "iris.rds", package="explore")
  data <- readRDS(file = file)
  tibble::as_tibble(data)
}

#' Use the mtcars data set
#'
#' This data set comes with base R. The data was extracted from
#' the 1974 Motor Trend US magazine, and comprises fuel consumption and
#' 10 aspects of automobile design and performance for 32 automobiles
#' (1973–74 models).
#' @return Dataset
#' @examples
#' use_data_mtcars()
#' @export

use_data_mtcars <- function() {
  file <- system.file("extdata", "mtcars.rds", package="explore")
  data <- readRDS(file = file)
  return(tibble::as_tibble(data))
}

#' Use the mpg data set
#'
#' This data set comes with the ggplot2 package.
#' It contains a subset of the fuel economy data that the EPA makes available on
#' https://fueleconomy.gov/. It contains only models which had a new
#' release every year between 1999 and 2008 - this was used as a proxy for the
#' popularity of the car.
#' @return Dataset
#' @seealso [`ggplot2::mpg`]
#' @examples
#' use_data_mpg()
#' @export

use_data_mpg <- function() {
  return(ggplot2::mpg)
}

#' Use the diamonds data set
#'
#' This data set comes with the ggplot2 package.
#' It contains the prices and other attributes of almost 54,000 diamonds.
#' @return Dataset
#' @seealso [`ggplot2::diamonds`]
#' @examples
#' use_data_diamonds()
#' @export

use_data_diamonds <- function() {
  return(ggplot2::diamonds)
}

#' Use the starwars data set
#'
#' This data set comes with the dplyr package.
#' It contains data of 87 star war characters
#' @return Dataset
#' @seealso [`dplyr::starwars`]
#' @examples
#' use_data_starwars()
#' @export

use_data_starwars <- function() {
  return(dplyr::starwars)
}

#' Use the penguins data set
#'
#' This data set comes with the palmerpenguins package.
#' It contains measurements for penguin species, island in Palmer Archipelago,
#' size (flipper length, body mass, bill dimensions), and sex.
#'
#' @return Dataset
#' @seealso [`palmerpenguins::penguins`]
#' @examples
#' use_data_penguins()
#' @export

use_data_penguins <- function() {
  return(palmerpenguins::penguins)
}

#' Use the titanic data set
#'
#' This data set comes with base R.
#' Survival of passengers on the Titanic.
#'
#' @param count use count data
#' @return Dataset
#' @examples
#' use_data_titanic()
#' @export

use_data_titanic <- function(count = FALSE) {
  n <- NULL
  file <- system.file("extdata", "titanic.rds", package = "explore")
  data <- readRDS(file = file)

  data <- tibble::as_tibble(data)

  if (!count) {
    data <- uncount_compat(dat = data, wt = n)
  }

  return(data)
}

#' Use the beer data set
#'
#' This data set is an incomplete collection of popular beers in
#' Austria, Germany and Switzerland. Data are collected from various
#' websites in 2023. Some of the collected data may be incorrect.
#'
#' @return Dataset as tibble
#' @examples
#' use_data_beer()
#' @export

use_data_beer <- function() {
  file <- system.file("extdata", "beer.rds", package="explore")
  data <- readRDS(file = file)
  tibble::as_tibble(data)
}
