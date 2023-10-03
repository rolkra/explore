test_that("Errors are handled properly.", {
  expect_error(
    abtest(tibble::tibble(x = character())),
    "must be a whole number larger than or equal to 1, not the number 0."
    )
})
