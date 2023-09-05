test_that("uncount has the correct result", {
  tit <- use_data_titanic(count = TRUE)
  expect_equal(nrow(uncount_compat(tit, wt = n)), 2201)
})
