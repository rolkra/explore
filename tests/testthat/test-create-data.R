context("create_data")
library(explore)

# create_data_empty -------------------------------------------------------

# create empty dataset with obs
test_that("create_data_empty()", {
  expect_equal(
    dim(create_data_empty(obs = 100)),
    c(100, 0)
  )
})

# error if obs < 1
test_that("create_data_empty()", {
  expect_error(create_data_empty(obs = -1))
})


# create data buy ---------------------------------------------------------

# create dataset with obs
test_that("create_data_buy()", {
  expect_equal(
    nrow(create_data_buy(obs = 100)),
    100
  )
})

# error if obs < 1
test_that("create_data_buy()", {
  expect_error(create_data_buy(obs = -1))
})

# create dataset with target as factor
test_that("create_data_buy()", {
  d <- create_data_buy(obs = 100, target_name = "target", factorise_target = TRUE)
  expect_equal(
    is.factor(d$target),
    TRUE
  )
})

# create dataset is reproducible when using the same seed
test_that("create_data_buy()", {
  expect_equal(
    create_data_buy(obs = 100, seed = 1),
    create_data_buy(obs = 100, seed = 1)
  )
})

# add_id works
test_that("create_data_buy()", {
  expect_true(
    ncol(create_data_buy(obs = 100, add_id = TRUE)) ==
      ncol(create_data_buy(obs = 100, add_id = FALSE)) + 1
  )
})


# create data person ------------------------------------------------------

# create dataset with obs
test_that("create_data_person()", {
  expect_equal(
    nrow(create_data_person(obs = 100)),
    100
  )
})

# error if obs < 1
test_that("create_data_person()", {
  expect_error(create_data_person(obs = -1))
})

# create dataset is reproducible when using the same seed
test_that("create_data_person()", {
  expect_equal(
    create_data_person(obs = 100, seed = 1),
    create_data_person(obs = 100, seed = 1)
  )
})

# add_id works
test_that("create_data_person()", {
  expect_true(
    ncol(create_data_person(obs = 100, add_id = TRUE)) ==
      ncol(create_data_person(obs = 100, add_id = FALSE)) + 1
  )
})

# create data app ------------------------------------------------------

# create dataset with obs
test_that("create_data_app()", {
  expect_equal(
    nrow(create_data_app(obs = 100)),
    100
  )
})

# error if obs < 1
test_that("create_data_app()", {
  expect_error(create_data_app(obs = -1))
})

# create dataset is reproducible when using the same seed
test_that("create_data_app()", {
  expect_equal(
    create_data_app(obs = 100, seed = 1),
    create_data_app(obs = 100, seed = 1)
  )
})

# add_id works
test_that("create_data_app()", {
  expect_true(
    ncol(create_data_app(obs = 100, add_id = TRUE)) ==
    ncol(create_data_app(obs = 100, add_id = FALSE)) + 1
  )
})

# create data churn ------------------------------------------------------

# create dataset with obs
test_that("create_data_churn()", {
  expect_equal(
    nrow(create_data_churn(obs = 100)),
    100
  )
})

# error if obs < 1
test_that("create_data_churn()", {
  expect_error(create_data_churn(obs = -1))
})

# create dataset is reproducible when using the same seed
test_that("create_data_churn()", {
  expect_equal(
    create_data_churn(obs = 100, seed = 1),
    create_data_churn(obs = 100, seed = 1)
  )
})

# add_id works
test_that("create_data_churn()", {
  expect_true(
    ncol(create_data_churn(obs = 100, add_id = TRUE)) ==
      ncol(create_data_churn(obs = 100, add_id = FALSE)) + 1
  )
})

# create dataset with target as factor
test_that("create_data_churn()", {
  d <- create_data_churn(obs = 100, target_name = "target", factorise_target = TRUE)
  expect_equal(
    is.factor(d$target),
    TRUE
  )
})

# create data newsletter ------------------------------------------------------

# create dataset with obs
test_that("create_data_newsletter()", {
  expect_equal(
    nrow(create_data_newsletter(obs = 100)),
    100
  )
})

# error if obs < 1
test_that("create_data_newsletter()", {
  expect_error(create_data_newsletter(obs = -1))
})

# create dataset is reproducible when using the same seed
test_that("create_data_newsletter()", {
  expect_equal(
    create_data_newsletter(obs = 100, seed = 1),
    create_data_newsletter(obs = 100, seed = 1)
  )
})

# add_id works
test_that("create_data_newsletter()", {
  expect_true(
    ncol(create_data_newsletter(obs = 100, add_id = TRUE)) ==
      ncol(create_data_newsletter(obs = 100, add_id = FALSE)) + 1
  )
})

# create data random ------------------------------------------------------

# create dataset with obs
test_that("create_data_random()", {
  expect_equal(
    nrow(create_data_random(obs = 100)),
    100
  )
})

# error if obs < 1
test_that("create_data_random()", {
  expect_error(create_data_random(obs = -1))
})

# create dataset is reproducible when using the same seed
test_that("create_data_random()", {
  expect_equal(
    create_data_random(obs = 100, seed = 1),
    create_data_random(obs = 100, seed = 1)
  )
})

# add_id works
test_that("create_data_random()", {
  expect_true(
    ncol(create_data_random(obs = 100, add_id = TRUE)) ==
      ncol(create_data_random(obs = 100, add_id = FALSE)) + 1
  )
})

# create dataset with target as factor
test_that("create_data_random()", {
  d <- create_data_random(obs = 100, target_name = "target", factorise_target = TRUE)
  expect_equal(
    is.factor(d$target),
    TRUE
  )
})

# create data unfair ------------------------------------------------------

# create dataset with obs
test_that("create_data_unfair()", {
  expect_equal(
    nrow(create_data_churn(obs = 100)),
    100
  )
})

# error if obs < 1
test_that("create_data_unfair()", {
  expect_error(create_data_unfair(obs = -1))
})

# create dataset is reproducible when using the same seed
test_that("create_data_unfair()", {
  expect_equal(
    create_data_churn(obs = 100, seed = 1),
    create_data_churn(obs = 100, seed = 1)
  )
})

# add_id works
test_that("create_data_unfair()", {
  expect_true(
    ncol(create_data_churn(obs = 100, add_id = TRUE)) ==
      ncol(create_data_churn(obs = 100, add_id = FALSE)) + 1
  )
})

# create dataset with target as factor
test_that("create_data_unfair()", {
  d <- create_data_churn(obs = 100, target_name = "target", factorise_target = TRUE)
  expect_equal(
    is.factor(d$target),
    TRUE
  )
})
