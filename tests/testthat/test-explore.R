# data contains no NA -----------------------------------------------------

# num variable
test_that("explore()", {
  data <- iris
  expect_no_error(explore(data, Sepal.Length))
})

test_that("explore_tbl()", {
  expect_no_error(explore_tbl(iris))
})

# cat variable
test_that("explore()", {
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

# cat variable, num target (boxplots)
test_that("explore()", {
  data <- iris
  data[1, ] <- NA
  expect_no_error(explore(data, Species, target = Sepal.Length))
})

# points xy
test_that("explore()", {
  data <- iris
  data[1, ] <- NA
  expect_no_error(explore(data, Sepal.Length, Sepal.Width, targetpct = 0.7))
})
