#' A/B testing
#'
#' @param data A dataset
#' @param expr Logical expression, that return in a FALSE/TRUE
#' @param target Target variable
#' @param sign_level Significance Level (typical 0.01/0.05/0.10)
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

abtest <- function(data, expr, target, sign_level = 0.05) {

  # check parameter
  check_data_frame_non_empty(data)
  rlang::check_required(target)
  rlang::check_required(expr)
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
    p <- abtest_targetpct(data, {{ expr }}, {{ target }}, sign_level)
  } else {
    # t test
    p <- abtest_targetnum(data, {{ expr }}, {{ target }}, sign_level)
  }

  # plot output
  p

} # abtest


#' A/B testing
#'
#' @param data A dataset
#' @param expr Expression, that results in a FALSE/TRUE
#' @param target Target variable (must be numeric)
#' @param sign_level Significance Level (typical 0.01/0.05/0.10)
#' @return Plot that shows if difference is significant
#' @examples
#' data <- create_data_buy(obs = 100)
#' abtest(data, city_ind == 1, target = age)

abtest_targetnum <- function(data, expr, target, sign_level = 0.05) {

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
  data_ab <- data |>
    dplyr::group_by({{ expr }}) |>
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
      fill = "grey") +
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


#' A/B testing
#'
#' @param data A dataset
#' @param expr Expression, that results in a FALSE/TRUE
#' @param target Target variable (must be 0/1 or FALSE/TRUE)
#' @param sign_level Significance Level (typical 0.01/0.05/0.10)
#' @return Plot that shows if difference is significant
#' @examples
#' data <- create_data_buy(obs = 100)
#' abtest(data, female_ind == 1, target = buy)
#' abtest(data, age >= 40, target = buy)

abtest_targetpct <- function(data, expr, target, sign_level = 0.05) {
  # parameter target
  rlang::check_required(target)
  target_quo <- enquo(target)
  target_txt <- quo_name(target_quo)[[1]]

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
  data_ab <- data %>%
    dplyr::group_by({{expr}}) %>%
    dplyr::summarise(
      n = dplyr::n(),
      target1_sum = sum({{target}}),
      target1_pct = mean({{target}} * 100)
    )

  # return if less than 2 groups!
  if (nrow(data_ab) < 2) {
    cat("Expression does not generate 2 groups (A/B)\n")
    return(NA)
  }

  # rename expression
  expression_txt <- names(data_ab)[1]
  names(data_ab)[1] <- "expression"

  # slice A/B groups
  a_grp <- data_ab %>% dplyr::filter(expression == TRUE)
  b_grp <- data_ab %>% dplyr::filter(expression == FALSE)

  m <- matrix(c(
    a_grp$target1_sum, b_grp$target1_sum,
    a_grp$n - a_grp$target1_sum, b_grp$n - b_grp$target1_sum),2,2)

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

  # plot result
  p <- data_ab %>%
    ggplot2::ggplot(ggplot2::aes(x = expression, y = target1_pct)) +
    ggplot2::geom_col(fill = "grey") +
    ggplot2::geom_hline(
      yintercept = data_ab$target1_pct, alpha = 0.3,
      linetype = "dashed", color = "red") +
    ggplot2::geom_text(ggplot2::aes(
      label = paste("n =", format_num_space(n), "\n\n", "target =", format_num_space(target1_sum))
    ), position = ggplot2::position_stack(vjust = 0.5),
    size = 3, color = "white") +
    ggplot2::geom_text(ggplot2::aes(
      label = round(target1_pct, 2),
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
