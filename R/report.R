#' Generate a report of all variables
#'
#' Generate a report of all variables
#' If target is defined, the relation to the target is reported
#'
#' @param data A dataset
#' @param n Weights variable for count data
#' @param target Target variable (0/1 or `FALSE`/`TRUE`)
#' @param targetpct Plot variable as target% (`FALSE`/`TRUE`)
#' @param split Alternative to targetpct (split = !targetpct)
#' @param output_file Filename of the html report
#' @param output_dir Directory where to save the html report
#' @examples
#' if (rmarkdown::pandoc_available("1.12.3")) {
#'  report(iris, output_dir = tempdir())
#'  }
#' @export

report <- function(data, n, target, targetpct, split, output_file, output_dir)  {

  check_data_frame_non_empty(data)
  # pandoc must be available to generate report
  # if RStudio is used, pandoc should be available
  rmarkdown::pandoc_available(version = "1.12.3", error = TRUE)

  # output_dir must be defined
  rlang::check_required(output_dir)

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
  output_file <- if (!missing(output_file)) {
    if (!identical(tools::file_ext(output_file), "html"))  {
      paste0(output_file, ".html")
    } else {
      output_file
    }
    } else if (is.na(target_text)) {
    "report_variable.html"
    } else {
    "report_target_split.html"
    }

  if (is.na(target_text) && is.na(n_txt))  {
    # report variables
    input_template <- "template_report_variable.Rmd"

  } else if (is.na(target_text) && !is.na(n_txt)) {
    # report variables + n
    input_template <- "template_report_variable_n.Rmd"
    var_name_n <- n_txt

  } else if (!is.na(target_text) && !is.na(n_txt)) {
    # report target + n
    input_template <- "template_report_target_split_n.Rmd"
    var_name_n <- n_txt
    var_name_target <- target_text  # needed in report template

  } else if (split) {
    # report target with split
    input_template <- "template_report_target_split.Rmd"
    var_name_target <- target_text  # needed in report template

  } else {
    # report target with percent
    input_template <- "template_report_target_pct.Rmd"

    if (output_file == "report_target_split.html") {
      # correct the generic output_file name.
      output_file <- "report_target.html"
    }
    var_name_target <- target_text # needed in report template

  }

  input_file <- system.file("extdata", input_template, package = "explore")

  cli::cli_alert_info("Processing template: {.val {input_template}}")
  report <- rmarkdown::render(
    input             = input_file,
    output_file       = output_file,
    output_dir        = output_dir,
    intermediates_dir = output_dir,
    clean = TRUE,
    quiet = TRUE
    )

  cli::cli_alert_success("Report created at {.file file://{report}}")
  invisible(report)# if
} # report

#' Generate a notebook
#'
#' Generate an RMarkdown Notebook template for a report. You must provide a
#' output-directory (parameter output_dir). The default file-name is
#' "notebook-explore.Rmd" (may overwrite existing file with same name)
#'
#' @param output_file Filename of the html report
#' @param output_dir Directory where to save the html report
#' @examplesIf rmarkdown::pandoc_available()
#' create_notebook_explore(output_file = "explore.Rmd", output_dir = tempdir())
#' @export

create_notebook_explore <- function(output_file = "notebook-explore.Rmd", output_dir) {

  # output_dir must be defined
  rlang::check_required(output_dir)

  # check if output-file has .Rmd extension
  # if not, add it!
  if (!missing(output_file)) {
    len <- nchar(output_file)
    if (tolower(substr(output_file, len-4, len)) != ".rmd")  {
      output_file <- paste0(output_file, ".Rmd")
    }
  } # if

  # get notebook template
  file_read <- system.file("extdata", "template_notebook_explore.Rmd", package="explore")
  file_write = path.expand(file.path(output_dir, output_file))

  # generate notebook
  success <- file.copy(from = file_read, to = file_write, overwrite = TRUE)

  if (success) {
    message(paste("Notebook", file_write, "generated"))
  } else {
    message(paste("Can not generate notebook", file_write))
  }

} # create_notebook_explore
