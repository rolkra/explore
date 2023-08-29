context("describe")
library(explore)


# describe ----------------------------------------------------------------

# describe returns a data frame
test_that("describe()", {
  expect_equal(is.data.frame(describe(iris)), TRUE)
})

# describe returns a row for each column in data frame
test_that("describe()", {
  expect_equal(ncol(iris), nrow(describe(iris)))
})

# first column contains names of all variables in data frame
test_that("describe()", {
  expect_equal(names(iris), describe(iris)[[1]])
})


# describe_all ------------------------------------------------------------

# describe is equal to describe_all
test_that("describe_all()", {
  expect_equal(describe_all(iris), describe(iris))
})


# describe_tbl-------------------------------------------------------------

# describe_tbl returns nrows
test_that("describe_tbl()", {
  expect_equal((describe_tbl(iris, out = "list")[["observations"]]), nrow(iris))
})

# describe_tbl returns ncol
test_that("describe_tbl()", {
  expect_equal((describe_tbl(iris, out = "list")[["variables"]]), ncol(iris))
})


# describe variable -------------------------------------------------------

# describe is equal to describe_num
test_that("describe_num()", {
  expect_equal(
    describe(iris, Sepal.Length, out = "list"),
    describe_num(iris, Sepal.Length, out = "list")
  )
})

# describe_num describes variable name
test_that("describe_num()", {
  expect_equal(
    describe_num(iris, Sepal.Length, out = "list")[["name"]],
    "Sepal.Length"
  )
})

# describe_num describes variable min
test_that("describe_num()", {
  expect_equal(
    describe_num(iris, Sepal.Length, out = "list")[["min"]],
    min(iris$Sepal.Length)
  )
})

# describe_num describes variable max
test_that("describe_num()", {
  expect_equal(
    describe_num(iris, Sepal.Length, out = "list")[["max"]],
    max(iris$Sepal.Length)
  )
})

# describe_num describes variable median
test_that("describe_num()", {
  expect_equal(
    describe_num(iris, Sepal.Length, out = "list")[["median"]],
    median(iris$Sepal.Length)
  )
})

# describe_cat describes variable name
test_that("describe_cat()", {
  expect_equal(
    describe_cat(iris, Species, out = "list")[["name"]],
    "Species"
  )
})

