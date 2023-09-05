test_that("Reports can be created", {
  skip_on_ci()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available("1.12.3"), message = "Pandoc is not available")
  expect_rmd_messages <- function(object) {
    expect_output(object) %>%
      expect_message("Output created") %>%
      expect_message("processing file") %>%
      expect_message("output file")
  }
  expect_rmd_messages(report_file <- report(iris, output_dir = tempdir()))
  expect_equal(basename(report_file), "report_variable.html")
  # Test this if it works to make sure output was reproducible (if it is problematic, delete it.)
  # Delete
  unlink(report_file)
})
