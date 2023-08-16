context("use_data")
library(explore)


# use data iris -----------------------------------------------------------

# use_data returns data.frame
test_that("use_data_iris()", {
  expect_equal(
    is.data.frame(use_data_iris()),
    TRUE
  )
})

# use_data returns data.frame with >0 rows
test_that("use_data_iris()", {
  expect_equal(
    nrow(use_data_iris()) > 0,
    TRUE
  )
})

# use data mtcars -----------------------------------------------------------

# use_data returns data.frame
test_that("use_data_mtcars()", {
  expect_equal(
    is.data.frame(use_data_mtcars()),
    TRUE
  )
})

# use_data returns data.frame with >0 rows
test_that("use_data_mtcars()", {
  expect_equal(
    nrow(use_data_mtcars()) > 0,
    TRUE
  )
})

# use data mpg -----------------------------------------------------------

# use_data returns data.frame
test_that("use_data_mpg()", {
  expect_equal(
    is.data.frame(use_data_mpg()),
    TRUE
  )
})

# use_data returns data.frame with >0 rows
test_that("use_data_mpg()", {
  expect_equal(
    nrow(use_data_mpg()) > 0,
    TRUE
  )
})

# use data diamonds -----------------------------------------------------------

# use_data returns data.frame
test_that("use_data_diamonds()", {
  expect_equal(
    is.data.frame(use_data_diamonds()),
    TRUE
  )
})

# use_data returns data.frame with >0 rows
test_that("use_data_diamonds()", {
  expect_equal(
    nrow(use_data_diamonds()) > 0,
    TRUE
  )
})

# use data penguins -------------------------------------------------------

# use_data returns data.frame
test_that("use_data_penguins()", {
  expect_equal(
    is.data.frame(use_data_penguins()),
    TRUE
  )
})

# use_data returns data.frame with >0 rows
test_that("use_data_penguins()", {
  expect_equal(
    nrow(use_data_penguins()) > 0,
    TRUE
  )
})

# use data titanic -------------------------------------------------------

# use_data returns data.frame
test_that("use_data_titanic()", {
  expect_equal(
    is.data.frame(use_data_titanic()),
    TRUE
  )
})

# use_data returns data.frame with >0 rows
test_that("use_data_titanic()", {
  expect_equal(
    nrow(use_data_titanic()) > 0,
    TRUE
  )
})

# use_data returns data.frame with >0 rows
test_that("use_data_titanic()", {
  expect_equal(
    ncol(use_data_titanic(count = FALSE)) + 1,
    ncol(use_data_titanic(count = TRUE))
  )
})
