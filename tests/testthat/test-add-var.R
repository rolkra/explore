context("add_var")
library(explore)


# add_var_id --------------------------------------------------------------

# add new random variable (default name "id")
test_that("add_var_id()", {
  expect_equal(
    names(add_var_id(iris)),
    c("id", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
})

# new random variable is numeric
test_that("add_var_id()", {
  expect_equal(
    is.numeric(add_var_id(iris)[[1]]),
    TRUE
  )
})

# new id variable is a sequence starting with 1
test_that("add_var_id()", {
  expect_equal(
    add_var_id(iris, name = "new")[["new"]],
    seq(from = 1, to = nrow(iris))
  )
})

# add new random variable called "id2"
test_that("add_var_id()", {
  expect_equal(
    names(add_var_id(iris, name = "id2")),
    c("id2", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
})

# do not overwrite existing variable
test_that("add_var_id", {
  expect_error(add_var_id(iris, name = "Species", overwrite = FALSE))
})

# force to overwrite existing variable
test_that("add_var_id()", {
  expect_equal(
    names(add_var_id(iris, name = "Species", overwrite = TRUE)),
    c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
})

# add_var_random_01 ------------------------------------------------------

# add new random variable called "random_01"
test_that("add_var_random_01()", {
  expect_equal(
    names(add_var_random_01(iris)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "random_01" )
  )
})

# new variable has no values < 0
test_that("add_var_random_01()", {
  expect_equal(
    min(add_var_random_01(iris, name = "new")[["new"]]) >= 0,
    TRUE
  )
})

# new variable has no values > 1
test_that("add_var_random_01()", {
  expect_equal(
    max(add_var_random_01(iris, name = "new")[["new"]]) <= 1,
    TRUE
  )
})

# do not overwrite existing variable (if overwrite = FALSE)
test_that("add_var_random_01()", {
  expect_error(
    add_var_random_01(iris, name = "Species", overwrite = FALSE)
  )
})

# add_var_random_int ------------------------------------------------------

# add new random variable called "random_int"
test_that("add_var_random_int()", {
  expect_equal(
    names(add_var_random_int(iris)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "random_int" )
  )
})

# type of new variable is integer
test_that("add_var_random_int()", {
  expect_equal(
    is.integer(add_var_random_int(iris, name = "new")[["new"]]),
    TRUE
  )
})

# do not overwrite existing variable (if overwrite = FALSE)
test_that("add_var_random_int()", {
  expect_error(
    add_var_random_int(iris, name = "Species", overwrite = FALSE)
  )
})

# new variable has no values < 0
test_that("add_var_random_int()", {
  expect_equal(
    min(add_var_random_int(iris, name = "new", min_val = -100)[["new"]]) >= -100,
    TRUE
  )
})

# new variable has no values > 1
test_that("add_var_random_int()", {
  expect_equal(
    max(add_var_random_int(iris, name = "new", max_val = 100)[["new"]]) <= 100,
    TRUE
  )
})


# add_var_random_dbl ------------------------------------------------------

# add new random variable called "random_dbl"
test_that("add_var_random_dbl()", {
  expect_equal(
    names(add_var_random_dbl(iris)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "random_dbl" )
  )
})

# type of new variable is double
test_that("add_var_random_dbl()", {
  expect_equal(
    is.double(add_var_random_dbl(iris, name = "new")[["new"]]),
    TRUE
  )
})

# do not overwrite existing variable (if overwrite = FALSE)
test_that("add_var_random_dbl()", {
  expect_error(
    add_var_random_dbl(iris, name = "Species", overwrite = FALSE)
  )
})

# new variable has no values < 0
test_that("add_var_random_dbl()", {
  expect_equal(
    min(add_var_random_dbl(iris, name = "new", min_val = -100.0)[["new"]]) >= -100,
    TRUE
  )
})

# new variable has no values > 1
test_that("add_var_random_dbl()", {
  expect_equal(
    max(add_var_random_dbl(iris, name = "new", max_val = 100.0)[["new"]]) <= 100,
    TRUE
  )
})

# add_var_random_cat() ----------------------------------------------------

# add new random variable called "random_cat"
test_that("add_var_random_cat()", {
  expect_equal(
    names(add_var_random_cat(iris)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "random_cat" )
  )
})

# add new random variable with character cat
test_that("add_var_random_cat()", {
  expect_equal(
    add_var_random_cat(iris, cat = "X", name = "cat")[["cat"]],
    rep("X", nrow(iris))
  )
})

# add new random variable with character cat
test_that("add_var_random_cat()", {
  expect_equal(
    add_var_random_cat(iris, cat = c(1), name = "cat")[["cat"]],
    rep(1, nrow(iris))
  )
})

# do not overwrite existing variable (if overwrite = FALSE)
test_that("add_var_random_cat()", {
  expect_error(
    add_var_random_cat(iris, name = "Species", overwrite = FALSE)
  )
})

# add random starsign -----------------------------------------------------

# add new random variable called "random starsign"
test_that("add_var_random_starsign()", {
  expect_equal(
    names(add_var_random_starsign(iris)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "random_starsign" )
  )
})

# add new random variable called "starsign"
test_that("add_var_random_starsign()", {
  expect_equal(
    names(add_var_random_starsign(iris, name = "starsign")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "starsign" )
  )
})

# do not overwrite existing variable (if overwrite = FALSE)
test_that("add_var_random_starsign()", {
  expect_error(
    add_var_random_starsign(iris, name = "Species", overwrite = FALSE)
  )
})
