test_that("Basic trees work", {
  skip_on_ci()
  data <- dplyr::select(dplyr::starwars, hair_color, height)
  res <- explain_tree(data, target = hair_color, out = "model")
  expect_s3_class(res, "rpart")
})
