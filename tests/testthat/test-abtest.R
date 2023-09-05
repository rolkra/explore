test_that("Basic AB Tests work", {
  data <- create_data_buy(obs = 100)
  expect_s3_class(abtest(data, female_ind == 1, target = buy), class = "ggplot")  # chi2 test
  expect_s3_class(abtest(data, city_ind == 1, target = age, sign_level = 0.01), class = "ggplot")    # t test
})
