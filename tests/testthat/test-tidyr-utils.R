test_that("uncount has the correct result", {
  tit <- use_data_titanic(count = TRUE)
  expect_equal(nrow(uncount_compat(tit, wt = n)), 2201)
  res <- tibble::tribble(~x, ~n,
                  1.2, 1,
                  3.4, 3,
                  1.2, 2,
                  .5, NA,
                  1.2, 0,
                  .5, 0) %>%
    uncount_compat(n)
  expect_equal(nrow(res), 6)
})
