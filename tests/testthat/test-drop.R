
# drop_var_no_variance ----------------------------------------------------

# drop reduces number of variables in data
test_that("drop_var_no_variance()", {

  data <- iris
  data$const <- 1

  expect_equal(
    names(drop_var_no_variance(data)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
})

# drop of variable has no effect on number of observations
test_that("drop_var_no_variance()", {

  data <- iris
  data$const <- 1

  expect_equal(
    nrow(drop_var_no_variance(data)),
    nrow(data)
  )
})

# drop all variables is still a data.frame
test_that("drop_var_no_variance()", {
  expect_true(
    is.data.frame(drop_var_no_variance(data.frame(a = c(1,1,1)))),
  )
})

# error if data is not a data.frame
test_that("drop_var_no_variance()", {
  expect_error(
    drop_var_no_variance(data = "text"),
  )
})

# drop_var_not_numeric ----------------------------------------------------

# drop variable that is not numeric
test_that("drop_var_not_numeric()", {
  expect_equal(
    names(drop_var_not_numeric(iris)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
})

# drop variable that is not numeric, but keep number of observations
test_that("drop_var_not_numeric()", {
  expect_equal(
    nrow(drop_var_not_numeric(iris)),
    nrow(iris)
  )
})

# drop all variables is still a data.frame
test_that("drop_var_not_numeric()", {
  expect_true(
    is.data.frame(drop_var_not_numeric(data.frame(a = LETTERS[1:5]))),
  )
})

# error if data is not a data.frame
test_that("drop_var_not_numeric()", {
  expect_error(
    drop_var_not_numeric(data = "text"),
  )
})

# drop_var_with_na --------------------------------------------------------

# drop variable containing only NA values
test_that("drop_var_with_na()", {

  data <- iris
  data$Species <- NA

  expect_equal(
    names(drop_var_with_na(data)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
})

# drop variable containing only NA values, but keeps nuber of observations
test_that("drop_var_with_na()", {

  data <- iris
  data$Species <- NA

  expect_equal(
    nrow(drop_var_with_na(data)),
    nrow(data)
  )
})

# drop variable containing some NA values
test_that("drop_var_with_na()", {

  data <- iris
  data[1,"Species"] <- NA

  expect_equal(
    names(drop_var_with_na(data)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
})

# drop all variables is still a data.frame
test_that("drop_var_with_na()", {
  expect_true(
    is.data.frame(drop_var_with_na(data.frame(a = c(NA, NA, NA)))),
  )
})

# error if data is not a data.frame
test_that("drop_var_with_na()", {
  expect_error(
    drop_var_with_na(data = "text"),
  )
})

# drop_obs_with_na --------------------------------------------------------

# drop observations with NA values
test_that("drop_obs_with_na()", {

  data <- iris
  data[1 ,] <- NA

  expect_equal(
    nrow(drop_obs_with_na(data)),
    nrow(data) -1
  )
})

# drop observations with NA values, but keeps variables
test_that("drop_obs_with_na()", {

  data <- iris
  data[1, 1] <- NA

  expect_equal(
    ncol(drop_obs_with_na(data)),
    ncol(data)
  )
})

# drop all variables is still a data.frame
test_that("drop_obs_with_na()", {
  expect_true(
    is.data.frame(drop_obs_with_na(data.frame(a = c(NA, NA, NA)))),
  )
})

# error if data is not a data.frame
test_that("drop_obs_with_na()", {
  expect_error(
    drop_obs_with_na(data = "text"),
  )
})

# drop_obs_if -------------------------------------------------------------

# drop observations works
test_that("drop_obs_if()", {

  expect_equal(
    nrow(drop_obs_if(iris, Species == "setosa")),
    100
  )
})

# drop observations works (no dropping)
test_that("drop_obs_if()", {

  expect_equal(
    nrow(drop_obs_if(iris, Species == "not defined")),
    nrow(iris)
  )
})

# drop all observations is still a data.frame
test_that("drop_obs_if()", {

  expect_true(
    is.data.frame(drop_obs_if(iris, Sepal.Length >= 0))
  )
})

# drop_var_low_variance ---------------------------------------------------

# drop reduces number of variables in data (no variance)
test_that("drop_var_low_variance()", {

  data <- iris
  data$const <- 1

  expect_equal(
    names(drop_var_low_variance(data)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
})

# drop reduces number of variables in data (low variance)
test_that("drop_var_low_variance()", {

  data <- iris
  data$const <- 1
  data[1, "const"] <- 2

  expect_equal(
    names(drop_var_low_variance(data, max_prop = 0.99)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
})

# drop reduces NOT number of variables in data (low variance, but high max_prop)
test_that("drop_var_low_variance()", {

  data <- iris
  data$const <- 1
  data[1, "const"] <- 2

  expect_equal(
    names(drop_var_low_variance(data, max_prop = 0.999)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "const")
  )
})

# drop all variables is still a data.frame
test_that("drop_var_low_variance()", {
  expect_true(
    is.data.frame(drop_var_low_variance(data.frame(a = c(1,1,1))))
  )
})

# error if data is not a data.frame
test_that("drop_var_low_variance()", {
  expect_error(
    drop_var_low_variance(data = "text"),
  )
})

# drop_var_by_names -------------------------------------------------------

# drop variable by name (one)
test_that("drop_var_by_names()", {
  expect_equal(
    names(drop_var_by_names(iris, "Species")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
})

# drop variable by name (multiple)
test_that("drop_var_by_names()", {
  expect_equal(
    names(drop_var_by_names(iris, c("Species", "Petal.Width"))),
    c("Sepal.Length", "Sepal.Width", "Petal.Length")
  )
})

# drop all variables is still a data.frame
test_that("drop_var_by_names()", {
  expect_true(
    is.data.frame(drop_var_by_names(iris, names(iris))),
  )
})

# drop variable creates NO error if variable name not in data
test_that("drop_var_by_names()", {
  expect_equal(
    names(drop_var_by_names(iris, "NotExist")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
})

# error if data is not a data.frame
test_that("drop_var_by_names()", {
  expect_error(
    drop_var_by_names(data = "text"),
  )
})
