# data contains no NA -----------------------------------------------------

test_that("explore() works with numeric variables", {
  data <- iris
  expect_no_error(explore(data, Sepal.Length))
})

test_that("explore_tbl()", {
  expect_no_error(explore_tbl(iris))
})

test_that("explore() works with categorical variables", {
  data <- iris
  expect_no_error(explore(data, Species))
})

# num variable, cat target (histograms)
test_that("explore()", {
  data <- iris
  expect_no_error(explore(data, Sepal.Length, target = Species))
})

# cat variable, num target (boxplots)
test_that("explore()", {
  data <- iris
  expect_no_error(explore(data, Species, target = Sepal.Length))
})

# points xy
test_that("explore()", {
  data <- iris
  expect_no_error(explore(data, Sepal.Length, Sepal.Width))
})

# data contains NA  -------------------------------------------------------

# num variable
test_that("explore()", {
  data <- iris
  data[1, ] <- NA
  expect_no_error(explore(data, Sepal.Length))
})

# cat variable
test_that("explore()", {
  data <- iris
  data[1, ] <- NA
  expect_no_error(explore(data, Species))
})

# num variable, cat target (histograms)
test_that("explore()", {
  data <- iris
  data[1, ] <- NA
  expect_no_error(explore(data, Sepal.Length, target = Species))
})

test_that("explore() works with cat variables and num targets.", {
  # Creates a boxplot
  data <- iris
  data[1, ] <- NA
  expect_no_error(p <- explore(data, Species, target = Sepal.Length))
  expect_s3_class(p$layers[[1]]$geom, "GeomBoxplot")
})

test_that("explore() works with xy points", {
  data <- iris
  data[1, ] <- NA
  expect_no_error(explore(data, Sepal.Length, Sepal.Width))
})
