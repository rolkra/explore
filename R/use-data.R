#' Use the iris flower data set
#'
#' This data set comes with base R. The data set gives the measurements
#' in centimeters of the variables sepal length and width and petal length and
#' width, respectively, for 50 flowers from each of 3 species of iris.
#' The species are Iris setosa, versicolor, and virginica.
#' @return Dataset
#' @examples
#' use_data_iris()
#' @export

use_data_iris <- function() {
  utils::data("iris")
  return(iris)
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
  utils::data("mtcars")
  return(mtcars)
}

#' Use the mpg data set
#'
#' This data set comes with the ggplot2 package.
#' It contains a subset of the fuel economy data that the EPA makes available on
#' https://fueleconomy.gov/. It contains only models which had a new
#' release every year between 1999 and 2008 - this was used as a proxy for the
#' popularity of the car.
#' @return Dataset
#' @examples
#' use_data_mpg()
#' @export

use_data_mpg <- function() {
  return(ggplot2::mpg)
}

#' Use the diamonds data set
#'
#' This data set comes with the ggplot2 package.
#' It containing the prices and other attributes of almost 54,000 diamonds.
#' @return Dataset
#' @examples
#' use_data_diamonds()
#' @export

use_data_diamonds <- function() {
  return(ggplot2::diamonds)
}

#' Use the penguins data set
#'
#' This data set comes with the palmerpenguins package.
#' It contains measurements for penguin species, island in Palmer Archipelago,
#' size (flipper length, body mass, bill dimensions), and sex.
#'
#' @return Dataset
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
  utils::data(Titanic)
  if (count) {
    data <- tibble::as_tibble(Titanic)
  } else {
    data <- tibble::as_tibble(Titanic)
    data <- tidyr::uncount(data = data, weights = n)
  }
  return(data)
}
