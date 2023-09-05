#' Add a random categorical variable to dataset
#'
#' @param data A dataset
#' @param name Name of new variable (as string)
#' @param cat Vector of categories
#' @param prob Vector of probabilities
#' @param overwrite Can new random variable overwrite an existing variable in dataset?
#' @param seed Seed for random number generation (integer)
#' @return Dataset containing new random variable
#' @examples
#' add_var_random_cat(iris)
#' add_var_random_cat(iris, name = "my_cat")
#' add_var_random_cat(iris, cat = c("Version A", "Version B"))
#' add_var_random_cat(iris, cat = c(1,2,3,4,5))
#' @export

add_var_random_cat <- function(data,
                               name = "random_cat",
                               cat = LETTERS[1:6],
                               prob,
                               overwrite = TRUE,
                               seed) {

  # data table available?
  check_data_frame_non_empty(data)

  # check if var already exists
  if (name %in% names(data) & !overwrite) {
    stop("Variable ", name, " already exists!")
  }

  # check prob
  if (missing(prob)) {
    prob <- rep(1, length(cat))
  }
  diff <- length(cat) - length(prob)
  if (diff > 0) {
    prob <- c(prob, rep(0, diff))
  }

  # check seed
  if (!missing(seed)) {
    set.seed(seed)
  }

  # create var
  data[[name]] <- sample(cat,
                         size = nrow(data),
                         prob = prob,
                         replace = TRUE)

  # return data
  data
} # add_var_random_cat


#' Add a random 0/1 variable to dataset
#'
#' @param data A dataset
#' @param name Name of new variable (as string)
#' @param prob Vector of probabilities
#' @param overwrite Can new random variable overwrite an existing variable in dataset?
#' @param seed Seed for random number generation (integer)
#' @return Dataset containing new random variable
#' @examples
#' add_var_random_01(iris)
#' add_var_random_01(iris, name = "my_var")
#' @export

add_var_random_01 <- function(data, name = "random_01", prob = c(0.5, 0.5), overwrite = TRUE, seed) {

  data <- add_var_random_cat(data,
                             name = name,
                             cat = c(0L, 1L),
                             prob = prob,
                             overwrite = overwrite,
                             seed = seed)
  data

} # add_var_random_01

#' Add a random integer variable to dataset
#'
#' @param data A dataset
#' @param name Name of new variable (as string)
#' @param min_val Minimum random integers
#' @param max_val Maximum random integers
#' @param overwrite Can new random variable overwrite an existing variable in dataset?
#' @param seed Seed for random number generation (integer)
#' @return Dataset containing new random variable
#' @examples
#' add_var_random_int(iris)
#' add_var_random_int(iris, name = "random_var")
#' add_var_random_int(iris, min_val = 1, max_val = 10)
#' add_var_random_int(iris, min_val = 1, max_val = 100, overwrite = FALSE)
#' @export

add_var_random_int <- function(data, name = "random_int",
                               min_val = 1, max_val = 10,
                               overwrite = TRUE,
                               seed) {

  # data table available?
  check_data_frame_non_empty(data)

  if (name %in% names(data) & !overwrite) {
    stop("Variable ", name, " already exists!")
  }

  # check seed
  if (!missing(seed)) {
    set.seed(seed)
  }

  # possible integer values
  values <- seq(from = min_val, to = max_val)

  # add random variable
  data[[name]] <- sample(values, nrow(data), replace = TRUE)

  # return data
  data

} # add_var_random_int


#' Add a random double variable to dataset
#'
#' @param data A dataset
#' @param name Name of new variable (as string)
#' @param min_val Minimum random integers
#' @param max_val Maximum random integers
#' @param overwrite Can new random variable overwrite an existing variable in dataset?
#' @param seed Seed for random number generation (integer)
#' @return Dataset containing new random variable
#' @examples
#' add_var_random_dbl(iris)
#' add_var_random_dbl(iris, name = "random_var")
#' add_var_random_dbl(iris, min_val = 1, max_val = 10)
#' add_var_random_dbl(iris, min_val = 1, max_val = 100, overwrite = FALSE)
#' @export

add_var_random_dbl <- function(data, name = "random_dbl",
                               min_val = 0, max_val = 100,
                               overwrite = TRUE,
                               seed) {

  # data table available?
  check_data_frame_non_empty(data)

  # check variable name
  if (name %in% names(data) & !overwrite) {
    stop("Variable ", name, " already exists!")
  }

  # check seed
  if (!missing(seed)) {
    set.seed(seed)
  }

  # add random variable
  data[[name]] <- stats::runif(nrow(data), min = min_val, max = max_val)

  # return data
  data

} # add_var_random_dbl

#' Add a random moon variable to dataset
#'
#' @param data A dataset
#' @param name Name of new variable (as string)
#' @param overwrite Can new random variable overwrite an existing variable in dataset?
#' @param seed Seed for random number generation (integer)
#' @return Dataset containing new random variable
#' @examples
#' add_var_random_moon(iris)
#' @export

add_var_random_moon <- function(data, name = "random_moon", overwrite = TRUE, seed) {

  # add starsign
  moons <- c("New ( )", "Waxing (+)", "Full (O)", "Waning (-)")
  data <- add_var_random_cat(data,
                             cat = moons,
                             name = name,
                             prob = c(0.1, 0.4, 0.1, 0.4),
                             overwrite = overwrite,
                             seed = seed)
  # return data
  data

} # add_var_random_moon

#' Add a random starsign variable to dataset
#'
#' @param data A dataset
#' @param name Name of new variable (as string)
#' @param lang Language used for starsign (en = English, de = Deutsch, es = Espanol)
#' @param overwrite Can new random variable overwrite an existing variable in dataset?
#' @param seed Seed for random number generation (integer)
#' @return Dataset containing new random variable
#' @examples
#' add_var_random_starsign(iris)
#' @export

add_var_random_starsign <- function(data, name = "random_starsign", lang = "en", overwrite = TRUE, seed) {

  # add starsign
  signs <- c("Aries", "Taurus", "Gemini", "Cancer", "Leo",
             "Virgo", "Libra", "Scorpio", "Saggitarius",
             "Carpicorn", "Aquarius", "Pisces")

  if (lang == "de") {
    signs <- c("Widder", "Stier", "Zwilling", "Krebs", "Loewe",
               "Jungfrau", "Waage", "Skorpion", "Schuetze",
               "Steinbock", "Wassermann","Fische")
  }

  if (lang == "es") {
    signs <- c("Aries", "Tauro", "Geminis", "Cancer", "Leo",
               "Virgo", "Libra", "Escorpio", "Sagitario",
               "Capricornio", "Acuario", "Piscis")
  }

  data <- add_var_random_cat(data,
                             cat = signs,
                             name = name,
                             overwrite = overwrite,
                             seed = seed)
  # return data
  data

} # add_var_random_starsign


#' Add a variable id at first column in dataset
#'
#' @param data A dataset
#' @param name Name of new variable (as string)
#' @param overwrite Can new id variable overwrite an existing variable in dataset?
#' @return Dataset containing new id variable
#' @examples
#' add_var_id(iris)
#' @export

add_var_id <- function(data, name = "id", overwrite = FALSE)  {
  # data table available?
  check_data_frame(data)

  # check variable name
  if (name %in% names(data) & !overwrite) {
    stop("Variable ", name, " already exists!")
  }

  # drop var if overwrite == TRUE
  if (name %in% names(data) & overwrite) {
    data[[name]] <- NULL
  }

  # create id
  data <- data.frame(seq(from = 1, to = nrow(data)), data)
  names(data)[1] <- name
  data

} # add_var_id
