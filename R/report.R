#' Generate a report of all variables
#'
#' Generate a report of all variables
#' If target is defined, the relation to the target is reported
#'
#' @param data A dataset
#' @param n Weights variable for count data
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param targetpct Plot variable as target% (FALSE/TRUE)
#' @param split Alternative to targetpct (split = !targetpct)
#' @param output_file Filename of the html report
#' @param output_dir Directory where to save the html report
#' @import rmarkdown
#' @examples
#' if (rmarkdown::pandoc_available("1.12.3"))   {
#'   report(iris, output_dir = tempdir())
#' }
#' @export

report <- function(data, n, target, targetpct, split, output_file, output_dir)  {

  # pandoc must be available to generate report
  # if RStudio is used, pandoc should be available
  if (!rmarkdown::pandoc_available())  {
    stop("no report generated because pandoc is not available")
  }

  # pandoc version 1.12.3 or higher must be available
  if (!rmarkdown::pandoc_version() >= "1.12.3")  {
    stop("no report generated because pandoc version >= 1.12.3 needed")
  }

  # output_dir must be defined
  if(missing(output_dir)) {
    stop("output_dir must be defined")
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_text <- quo_name(target_quo)[[1]]
  } else {
    target_quo = NA
    target_text = NA
  }

  # parameter targetpct & split (set default value)
  if (missing(targetpct)) {
    if (missing(split)) {
      split = TRUE
    }
  } else {
    split = !targetpct
  }

  # parameter n
  if(!missing(n))  {
    n_quo <- enquo(n)
    n_txt <- quo_name(n_quo)[[1]]
  } else {
    n_txt = NA
  }

  # check if output-file has .html extension
  # if not, add it!
  if (!missing(output_file)) {
    len <- nchar(output_file)
    if (tolower(substr(output_file, len-4, len)) != ".html")  {
      output_file <- paste0(output_file, ".html")
    }
  } # if

  # report variables
  if(is.na(target_text) & is.na(n_txt))  {
    input_file <- system.file("extdata", "template_report_variable.Rmd", package="explore")
    if (missing(output_file)) {output_file = "report_variable.html"}
      rmarkdown::render(input = input_file,
                        output_file = output_file,
                        output_dir = output_dir,
                        intermediates_dir = output_dir,
                        clean = TRUE
    )

  # report variables + n
  } else if(is.na(target_text) & !is.na(n_txt))  {
    input_file <- system.file("extdata", "template_report_variable_n.Rmd", package="explore")
    if (missing(output_file)) {output_file = "report_variable.html"}
      var_name_n <- n_txt
      rmarkdown::render(input = input_file,
                        output_file = output_file,
                        output_dir = output_dir,
                        intermediates_dir = output_dir,
                        clean = TRUE
      )

      # report target + n
  } else if(!is.na(target_text) & !is.na(n_txt))  {
    input_file <- system.file("extdata", "template_report_target_split_n.Rmd", package="explore")
    if (missing(output_file)) {output_file = "report_target_split.html"}
      var_name_n <- n_txt
      var_name_target <- target_text  # needed in report template
      rmarkdown::render(input = input_file,
                        output_file = output_file,
                        output_dir = output_dir,
                        intermediates_dir = output_dir,
                        clean = TRUE
    )

    # report target with split
  } else if(split == TRUE)  {
    input_file <- system.file("extdata", "template_report_target_split.Rmd", package="explore")
    if (missing(output_file)) {output_file = "report_target_split.html"}
    var_name_target <- target_text  # needed in report template
    rmarkdown::render(input = input_file,
                      output_file = output_file,
                      output_dir = output_dir,
                      intermediates_dir = output_dir,
                      clean = TRUE
    )

    # report target with percent
  } else {
    input_file <- system.file("extdata", "template_report_target_pct.Rmd", package="explore")
    if (missing(output_file)) {output_file = "report_target.html"}
    var_name_target <- target_text # needed in report template
    rmarkdown::render(input = input_file,
                      output_file = output_file,
                      output_dir = output_dir,
                      intermediates_dir = output_dir,
                      clean = TRUE
    )
  } # if
} # report
