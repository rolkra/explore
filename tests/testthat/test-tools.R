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
