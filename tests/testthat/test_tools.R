context("tools")
library(explore)


# weight_target -----------------------------------------------------------

# weight target() with rare target_ind = 1
test_that("weight_target()", {
  expect_equal(weight_target(data.frame(id = 1:5, target_ind = c(0, 0, 0, 0, 1)),
                              target = target_ind),
               c(1, 1, 1, 1, 4)
               )
})

# weight target() with rare target_ind = 0
test_that("weight_target()", {
  expect_equal(weight_target(data.frame(id = 1:5, target_ind = c(1, 1, 1, 1, 0)),
                             target = target_ind),
               c(1, 1, 1, 1, 4)
  )
})


# add_random_var ----------------------------------------------------------

# add new random variable called "random"
test_that("add_random_var()", {
  expect_equal(
    names(add_random_var(iris)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "random" )
  )
})

# add new random variable called "random2"
test_that("add_random_var()", {
  expect_equal(
    names(add_random_var(iris, name = "random2")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "random2" )
  )
})

# do not overwrite existing variable (if overwrite = FALSE)
test_that("add_random_var()", {
  expect_error(
    add_random_var(iris, name = "Species", overwrite = FALSE)
  )
})
