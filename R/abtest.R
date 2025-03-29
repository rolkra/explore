#' A/B testing
#'
#' @param data A dataset. If no data is provided, a shiny app is launched
#' @param expr Logical expression, that return in a FALSE/TRUE
#' @param n A Variable for number of observations (count data)
#' @param target Target variable
#' @param sign_level Significance Level (typical 0.01/0.05/0.10)
#' @param color Fill color of bar/violin-plot
#' @return Plot that shows if difference is significant
#' @examples
#' ## Using chi2-test or t-test depending on target type
#' data <- create_data_buy(obs = 100)
#' abtest(data, female_ind == 1, target = buy)  # chi2 test
#' abtest(data, city_ind == 1, target = age)    # t test
#'
#' ## If small number of observations, Fisher's Exact test
#' ## is used for a binary target (if <= 5 observations in a subgroup)
#' data <- create_data_buy(obs = 25, seed = 1)
#' abtest(data, female_ind == 1, target = buy)  # Fisher's Exact test
#' @export

abtest <- function(data, expr, n, target, sign_level = 0.05, color = "grey") {

  ## define variables to pass CRAN tests
  group <- NULL
  nn <- NULL
  success <- NULL

  # if not data is provided, start shiny app
  if (missing(data)) {
    abtest_shiny()
    #return("hallo welt")
  } else if (all(c("group", "success", "n") %in% names(data)) & missing(expr)) {

    data$nn <- data$n
    abtest_targetpct(data = data,
                     expr = group == "B",
                     n = nn,
                     target = success,
                     sign_level = as.numeric(sign_level),
                     group_label = "Group",
                     ab_label = TRUE,
                     color = color)

  } else if (all(c("group", "success") %in% names(data)) & missing(expr)) {

    abtest_targetpct(data = data,
                     expr = group == "B",
                    #n = nn,
                     target = success,
                     sign_level = as.numeric(sign_level),
                     group_label = "Group",
                     ab_label = TRUE,
                     color = color)
  } else {
  # check parameter
  check_data_frame_non_empty(data)
  rlang::check_required(expr)
  rlang::check_required(target)
  check_number_decimal(sign_level, min = 0, max = 1)

  # parameter target
  target_quo <- enquo(target)
  target_txt <- quo_name(target_quo)[[1]]
  if (!target_txt %in% names(data)) {
    stop(paste0("target variable '", target_txt, "' not found in data"))
  }

  # check if target is cat/num
  type <- guess_cat_num(data[[target_txt]])

  if (type == "oth") {
    stop("Unknown type of target")
  }

  if (type == "cat") {
    # chi2 test, fishers exact test
    if (missing(n)) {
      p <- abtest_targetpct(data = data,
                            expr = {{ expr }},
                            target = {{ target }},
                            sign_level = sign_level,
                            color = color)
    } else {
      p <- abtest_targetpct(data = data,
                            expr = {{ expr }},
                            n = {{ n }},
                            target = {{ target }},
                            sign_level = sign_level,
                            color = color)

    } # missing(n)

  } else {
    # t test
    p <- abtest_targetnum(data = data,
                          expr = {{ expr }},
                          target = {{ target }},
                          sign_level = sign_level,
                          color = color)
  }

  # plot output
  p

  } # missing data
} # abtest


#' A/B testing comparing two mean
#'
#' @param data A dataset
#' @param expr Expression, that results in a FALSE/TRUE
#' @param target Target variable (must be numeric)
#' @param sign_level Significance Level (typical 0.01/0.05/0.10)
#' @param color fill color
#' @return Plot that shows if difference is significant
#' @examples
#' data <- create_data_buy(obs = 100)
#' abtest(data, city_ind == 1, target = age)

abtest_targetnum <- function(data, expr, target, sign_level = 0.05, color = "grey") {

  rlang::check_required(target)
  # parameter target
  target_quo <- enquo(target)
  target_txt <- quo_name(target_quo)[[1]]

  # define variables for CRAN-package check
  target_mean <- NULL
  target_median <- NULL

  # remove extreme values
  target_val <- data[[target_txt]]
  r <- quantile(target_val, c(0.01, 0.99), na.rm = TRUE)
  min_val <- r[1]
  max_val <- r[2]
  nrow_ori <- nrow(data)

  if (nrow_ori > 100) {
    data <- data %>%
      dplyr::filter(!!target_quo >= min_val) %>%
      dplyr::filter(!!target_quo <= max_val)
  }

  nrow_new <- nrow(data)
  if (nrow_new < nrow_ori) {
    caption_txt <- paste(
      format_num_space(nrow_ori - nrow_new),
      "observations with extreme target-values are excluded (top/bottom 1%)")
  } else {
    caption_txt <- ""
  }

  # expression
  data_ab <- data %>%
    dplyr::group_by({{ expr }}) %>%
    dplyr::summarise(
      target_mean = mean({{ target }}),
      target_median = median({{ target }}),
      n = dplyr::n())

  # return if less than 2 groups!
  if (nrow(data_ab) < 2) {
    cat("Expression does not generate 2 groups (A/B)\n")
    return(NA)
  }

  # rename expression
  expression_txt <- names(data_ab)[1]
  names(data_ab)[1] <- "expression"

  # slice A/B groups
  a_val <- data %>% dplyr::filter(!{{ expr }}) %>% dplyr::pull({{ target }})
  b_val <- data %>% dplyr::filter({{ expr }}) %>% dplyr::pull({{ target }})

  # t test
  result <- stats::t.test(
    x = a_val,
    y = b_val,
    var.equal = FALSE,
    na.action = "na.omit",
    alternative = "two.sided")

  # result text
  if (result$p.value <= sign_level)  {
    result_txt <- paste0(
      "SIGNIFICANT, p value = ", round(result$p.value, 3),
      " (max ", round(sign_level, 2), ")")
    result_lgl <- TRUE
  } else  {
    result_txt <- paste0(
      "NOT significant, p value = ", round(result$p.value, 3),
      " (max ", round(sign_level, 2), ")")
    result_lgl <- FALSE
  }

  test_used <- "t-test two sided"

  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = {{ expr }}, y = {{ target }})
  ) +
    ggplot2::geom_violin(
      fill = color) +
    #notch = TRUE,
    #outlier.shape = NA) +
    ggplot2::stat_summary(
      fun = "mean", geom = "point",
      color = "black", size = 2, shape = 1) +
    #ggplot2::ylim(c(min_val, max_val)) +
    ggplot2::geom_hline(
      yintercept = data_ab$target_mean, alpha = 0.3,
      linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = paste0("A/B test (", test_used, ")"),
      subtitle = result_txt,
      y = paste0("Mean target (", target_txt, ")"),
      x = expression_txt,
      caption = caption_txt
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect("white"),
      panel.grid.major = ggplot2::element_line("grey85"),
      panel.grid.minor = ggplot2::element_line("grey85"),
      panel.border = ggplot2::element_rect(fill = NA, color = "lightgrey"),
      plot.caption = ggplot2::element_text(hjust = 0.5))

  p

  p + ggplot2::geom_text(
    data = data_ab,
    ggplot2::aes(
      x = expression,
      y = target_median,
      label = paste("n =", format_num_space(n), "\n\n", "target =", format_num_space(target_mean))
    ), size = 3, color = "white")

} # abtest_targetnum


#' A/B testing comparing percent per group
#'
#' @param data A dataset
#' @param expr Expression, that results in a FALSE/TRUE
#' @param n A Variable for number of observations (count data)
#' @param target Target variable (must be 0/1 or FALSE/TRUE)
#' @param sign_level Significance Level (typical 0.01/0.05/0.10)
#' @param group_label Label of groups (default = expr)
#' @param ab_label Label Groups as A and B (default = FALSE)
#' @param color color of bar
#' @return Plot that shows if difference is significant
#' @examples
#' data <- create_data_buy(obs = 100)
#' abtest(data, female_ind == 1, target = buy)
#' abtest(data, age >= 40, target = buy)

abtest_targetpct <- function(data, expr, n, target, sign_level = 0.05, group_label, ab_label = FALSE, color = "grey") {

  # parameter target
  rlang::check_required(target)
  target_quo <- enquo(target)
  target_txt <- quo_name(target_quo)[[1]]

  # define variables for CRAN-package check
  success_n <- NULL

  # parameter n
  if (!missing(n)) {
    n_quo <- enquo(n)
    n_text <- quo_name(n_quo)[[1]]
    if (!n_text %in% names(data))  {
      stop(paste0("variable '", n_text, "' not found"))
    }

    if(any(is.na(data[[n_text]]))) {
      p <- plot_text("Undefined values", ggplot = TRUE)
      return(p)
    }

  } # if

  # define variables for CRAN-package check
  target1_sum <- NULL
  target1_pct <- NULL

  # format target as 0/1
  target_ori <- data[[target_txt]]
  data[[target_txt]] <- format_target(data[[target_txt]])
  if (any(data[[target_txt]] != target_ori)) {
    warning("target was convertet to binary (0/1 values)")
    caption_txt <- "Categorical target was converted to binary (0/1 values)"
  } else {
    caption_txt <- ""
  }

  # create table
  if (missing(n)) {
    data_ab <- data %>%
      dplyr::group_by({{ expr }}) %>%
      dplyr::summarise(
        n = dplyr::n(),
        target1_sum = sum({{ target }}),
        target1_pct = mean({{ target }} * 100)
      )
  } else {
    data_ab <- data %>%
      dplyr::mutate(success_n = {{ target }} * {{ n }}) %>%
      dplyr::group_by({{ expr }}) %>%
      dplyr::summarise(
        n = sum({{ n }}),
        target1_sum = sum(success_n),
        target1_pct = sum(success_n) / sum( {{n}} ) * 100
      )
  }

  # return if less than 2 groups!
  if (nrow(data_ab) < 2) {
    cat("Expression does not generate 2 groups (A/B)\n")
    return(NA)
  }

  # define group label (default = expression)
  expression_txt <- names(data_ab)[1]
  if(!missing(group_label)) {
    expression_txt <- group_label
  }
  names(data_ab)[1] <- "expression"

  # slice A/B groups
  a_grp <- data_ab %>% dplyr::filter(expression == TRUE)
  b_grp <- data_ab %>% dplyr::filter(expression == FALSE)

  m <- matrix(c(
    a_grp$target1_sum, b_grp$target1_sum,
    a_grp$n - a_grp$target1_sum, b_grp$n - b_grp$target1_sum),2,2)

  # check if meaningful
  if(a_grp$n < a_grp$target1_sum) {
    p <- plot_text("Not meaningful values", ggplot = TRUE)
    return(p)
  }
  if(b_grp$n < b_grp$target1_sum) {
    p <- plot_text("Not meaningful values", ggplot = TRUE)
    return(p)
  }

  # test for significance
  if (any(m <= 5)) {
    test_used <- "Fisher's Exact"
    result <- stats::fisher.test(
      matrix(c(
        a_grp$target1_sum, b_grp$target1_sum,
        a_grp$n - a_grp$target1_sum, b_grp$n - b_grp$target1_sum),2,2))

  } else {
    test_used <- "Chi2"
    result <- stats::chisq.test(
      matrix(c(
        a_grp$target1_sum, b_grp$target1_sum,
        a_grp$n - a_grp$target1_sum, b_grp$n - b_grp$target1_sum),2,2))
  }

  # result text
  if (result$p.value <= sign_level)  {
    result_txt <- paste0(
      "SIGNIFICANT, p value = ", round(result$p.value, 3),
      " (max ", round(sign_level, 2), ")")
    result_lgl <- TRUE
  } else  {
    result_txt <- paste0(
      "NOT significant, p value = ", round(result$p.value, 3),
      " (max ", round(sign_level, 2), ")")
    result_lgl <- FALSE
  }

  # cat(result_txt, "\n")

  # use label A/B instead of FALSE/TRUE
  if (ab_label) {
    data_ab[[1]] <- c("A","B")
  }

  # plot result
  p <- data_ab %>%
    ggplot2::ggplot(ggplot2::aes(x = expression, y = target1_pct)) +
    ggplot2::geom_col(fill = color) +
    ggplot2::geom_hline(
      yintercept = data_ab$target1_pct, alpha = 0.3,
      linetype = "dashed", color = "red") +
    ggplot2::geom_text(ggplot2::aes(
      label = paste("n =", format_num_space(n), "\n\n", "target =", format_num_space(target1_sum))
    ), position = ggplot2::position_stack(vjust = 0.5),
    size = 3, color = "white") +
    ggplot2::geom_text(ggplot2::aes(
      label = paste0(round(target1_pct, 2), "%"),
      vjust = ifelse(target1_pct == 0, 0, 1.2)
    ), position = "stack", size = 3) +
    ggplot2::labs(
      title = paste0("A/B test (", test_used, ")"),
      subtitle = result_txt,
      y = paste0("% target (", target_txt, ")"),
      x = expression_txt,
      caption = caption_txt
    ) +
    ggplot2::ylim(c(0, max(data_ab$target1_pct) * 1.1)) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect("white"),
      panel.grid.major = ggplot2::element_line("grey85"),
      panel.grid.minor = ggplot2::element_line("grey85"),
      panel.border = ggplot2::element_rect(fill = NA, color = "lightgrey"),
      plot.caption = ggplot2::element_text(hjust = 0.5))

  p

} # abtest_targetpct


#' A/B testing interactive
#'
#' Launches a shiny app to A/B test
#'
#' @param size_a Size of Group A
#' @param size_b Size of Group B
#' @param success_a Success of Group A
#' @param success_b Success of Group B
#' @param success_unit "count" | "percent"
#' @param sign_level  Significance Level (typical 0.01/0.05/0.10)
#' @examples
#'
#' # Only run examples in interactive R sessions
#' if (interactive())  {
#'    abtest_shiny()
#' }

abtest_shiny <- function(size_a = 100, size_b = 100,
                         success_a = 10, success_b = 20,
                         success_unit = "percent",
                         sign_level = 0.05)  {

  # check if interactive session
  if (!interactive()) {
    warning("This function can only be used in an interactive R session")
    return(invisible())
  }

  # define variables for CRAN-package check
  group <- NULL
  success <- NULL

  # define ui
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h3("A/B test"),
        shiny::hr(),
        shiny::numericInput(inputId = "size_a",
                           label = "Size A",
                           value = size_a,
                           min = 0),
        shiny::numericInput(inputId = "size_b",
                            label = "Size B",
                            value = size_b,
                            min = 0),
        shiny::radioButtons(inputId = "success_unit",
                            label = "Success unit",
                            c("Count" = "count", "Percent" = "percent"),
                            selected = tolower(success_unit),
                            inline = FALSE,
                            width = NULL),
        shiny::numericInput(inputId = "success_a",
                            label = "Success A",
                            value = success_a,
                            min = 0),
        shiny::numericInput(inputId = "success_b",
                            label = "Success B",
                            value = success_b,
                            min = 0),
        shiny::radioButtons(inputId = "sign_level",
                            label = "Significance level",
                            c("1%" = 0.01, "5%" = 0.05, "10%" = 0.10, "20%" = 0.20),
                            selected = sign_level,
                            inline = FALSE,
                            width = NULL)

        , width = 3),  #sidebarPanel

      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Plot",
                          shiny::plotOutput("graph", height = 300),
                          shiny::verbatimTextOutput("text")
          )) # tabsetPanel
        , width = 9) # mainPanel
    ) # sidebarLayout
  ) # fluidPage

  # server: calculate statistics and generate plot
  server <- function(input, output, session) {

    output$graph <- shiny::renderPlot({

      if(input$success_unit == "percent") {
        success_a <- round(input$size_a * input$success_a / 100)
        success_b <- round(input$size_b * input$success_b / 100)
      } else {
        success_a <- input$success_a
        success_b <- input$success_b
      }

      data_ab <- data.frame(
        group = c("A", "B", "A", "B"),
        success = c(0, 0, 1, 1),
        n = c(input$size_a - success_a,
              input$size_b - success_b,
              success_a,
              success_b)
      )

      abtest_targetpct(data = data_ab,
                       expr = group == "B",
                       n = n,
                       target = success,
                       sign_level = as.numeric(input$sign_level),
                       group_label = "Group",
                       ab_label = TRUE)
    }) # renderPlot graph_target

    output$text <- shiny::renderPrint({
      cat("\n")
    }) # renderText

  } # server

  # run shiny app
  shiny::shinyApp(ui = ui, server = server)

} # abtest_shiny

