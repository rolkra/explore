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


# mix_color ---------------------------------------------------------------

# mix 2 colors and get 3
test_that("mix_color()", {
  expect_equal(mix_color("black", "white", n = 3),
               c("#000000", "#7F7F7F", "#FFFFFF")
  )
})

# mix 1 color and get 3
test_that("mix_color()", {
  expect_equal(mix_color("blue", n = 3),
               c("#00007F", "#0000FF", "#7F7FFF")
  )
})

# mix 1 color and get 5
test_that("mix_color()", {
  expect_equal(length(mix_color("blue", n = 5)),
               5
  )
})

# mix 1 color and get 5, middle color is the same as original
test_that("mix_color()", {
  expect_equal(mix_color("#0000FF", n = 5)[3],
               "#0000FF"
  )
})
