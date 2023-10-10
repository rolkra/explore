test_that("Reports can be created", {
  skip_on_ci()
  skip_if_not(rmarkdown::pandoc_available("1.12.3"), message = "Pandoc is not available")
  expect_cli_messages <- function(object) {
      expect_message(object, "Processing template") %>%
      expect_message("Report created")
  }
  tmpdir <- tempdir()
  expect_cli_messages(report_file <- report(iris, output_dir = tmpdir))
  expect_equal(basename(report_file), "report_variable.html")
  # Test this if it works to make sure output was reproducible (if it is problematic, delete it.)
  # Delete
  unlink(report_file)
  unlink(tmpdir)
})
