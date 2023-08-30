# use_data returns data.frame --------------
test_that("use_data returns a data frame.", {
  expect_s3_class(use_data_iris(), "data.frame")
  expect_s3_class(use_data_mtcars(), "data.frame")
  expect_s3_class(use_data_mpg(), "data.frame")
  expect_s3_class(use_data_diamonds(), "data.frame")
  expect_s3_class(use_data_penguins(), "data.frame")
  expect_s3_class(use_data_titanic(), "data.frame")
})

# use_data returns data.frame with >0 rows ----------
test_that("use_data returns data frames with >0 rows", {
  expect_gt(nrow(use_data_iris()), 0)
  expect_gt(nrow(use_data_mtcars()), 0)
  expect_gt(nrow(use_data_mpg()), 0)
  expect_gt(nrow(use_data_diamonds()), 0)
  expect_gt(nrow(use_data_penguins()), 0)
  expect_gt(nrow(use_data_titanic()), 0)
})

# use_data returns data.frame with >0 rows
test_that("use_data_titanic() has the correct number of columns", {
  expect_equal(
    ncol(use_data_titanic(count = FALSE)) + 1,
    ncol(use_data_titanic(count = TRUE))
  )
})
