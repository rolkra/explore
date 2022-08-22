context("create_data")
library(explore)

# create_data_empty -------------------------------------------------------

# create empty dataset
test_that("create_data_empty()", {
  expect_equal(
    dim(create_data_empty(obs = 1000)),
    c(1000, 0)
  )
})

# create data buy ---------------------------------------------------------

# create dataset with obs
test_that("create_data_buy()", {
  expect_equal(
    nrow(create_data_buy(obs = 1000)),
    1000
  )
})

# create dataset with target as factor
test_that("create_data_buy()", {
  d <- create_data_buy(obs = 1000, target_name = "target", factorise_target = TRUE)
  expect_equal(
    is.factor(d$target),
    TRUE
  )
})

# create dataset is reproducible when using the same seed
test_that("create_data_buy()", {
  expect_equal(
    create_data_buy(obs = 1000, seed = 1),
    create_data_buy(obs = 1000, seed = 1)
  )
})


# create data person ------------------------------------------------------

# create dataset with obs
test_that("create_data_person()", {
  expect_equal(
    nrow(create_data_person(obs = 1000)),
    1000
  )
})

# create dataset is reproducible when using the same seed
test_that("create_data_person()", {
  expect_equal(
    create_data_person(obs = 1000, seed = 1),
    create_data_person(obs = 1000, seed = 1)
  )
})


# create data random ------------------------------------------------------

# create dataset with obs
test_that("create_data_random()", {
  expect_equal(
    nrow(create_data_random(obs = 1000)),
    1000
  )
})

# create dataset is reproducible when using the same seed
test_that("create_data_random()", {
  expect_equal(
    create_data_random(obs = 1000, seed = 1),
    create_data_random(obs = 1000, seed = 1)
  )
})
