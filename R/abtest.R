#' A/B testing
#'
#' @param data A dataset
#' @param expr Expression, that results in a FALSE/TRUE
#' @param target Target variable (must be 0/1 or FALSE/TRUE)
#' @param sign_level Significance Level (typical 0.01/0.05/0.10)
#' @return Plot that shows if difference is significant
#' @importFrom("stats", "chisq.test", "fisher.test")
#' @examples
#' data <- create_data_buy(obs = 100)
#' abtest(data, female_ind == 1, target = buy)
#' abtest(data, city_ind == 1, target = buy)
#' @export


abtest <- function(data, expr, target, sign_level = 0.05) {

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    stop("parameter target is missing")
    return(NA)
  }

  # define variables for CRAN-package check
  target1_sum <- NULL
  target1_pct <- NULL

  # format target as 0/1
  target_ori <- data[[target_txt]]
  data[[target_txt]] <- format_target(data[[target_txt]])
  if (any(data[[target_txt]] != target_ori)) {
    warning("target was convertet to binary (0/1 values)")
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
  if (nrow (data_ab) < 2) {
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
    result <- fisher.test(
      matrix(c(
        a_grp$target1_sum, b_grp$target1_sum,
        a_grp$n - a_grp$target1_sum, b_grp$n - b_grp$target1_sum),2,2))

  } else {
    test_used <- "Chi2"
    result <- chisq.test(
      matrix(c(
        a_grp$target1_sum, b_grp$target1_sum,
        a_grp$n - a_grp$target1_sum, b_grp$n - b_grp$target1_sum),2,2))
  }

  # result text
  if (result$p.value <= sign_level)  {
    result_txt <- paste0(
      "SIGNIFICANT, p value = ", round(result$p.value, 2),
      " (max ", round(sign_level, 2), ")")
    result_lgl <- TRUE
  } else  {
    result_txt <- paste0(
      "NOT significant, p value = ", round(result$p.value, 2),
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
      linetype = "dashed", color = "black") +
    ggplot2::geom_text(ggplot2::aes(
      label = paste("n =", format_num_space(n), "\n", "target =", format_num_space(target1_sum))
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
      x = expression_txt
    ) +
    ggplot2::ylim(c(0, max(data_ab$target1_pct) * 1.1)) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect("white"),
      panel.grid.major = ggplot2::element_line("grey85"),
      panel.grid.minor = ggplot2::element_line("grey85"),
      panel.border = ggplot2::element_rect(fill = NA, color = "lightgrey"))

  p

} # abtest
