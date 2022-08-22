context("tools")
library(explore)


# balance target ----------------------------------------------------------

# result should be reproducible if same seed is used
test_that("balance_target()", {
  data <- iris
  data$is_versicolor <- ifelse(data$Species == "versicolor", 1, 0)
  expect_equal(balance_target(data, target = is_versicolor, seed = 123),
               balance_target(data, target = is_versicolor, seed = 123)
  )
})

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
