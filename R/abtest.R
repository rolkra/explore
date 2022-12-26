#' A/B testing
#'
#' @param data A dataset
#' @param expr Expression, that results in a FALSE/TRUE
#' @param target Target variable (must be 0/1 or FALSE/TRUE)
#' @param sign_level Significance Level (typical 0.01/0.05/0.10)
#' @return Is difference significant? (FALSE/TRUE)
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
      target1_pct = mean({{target}})
    )

  print(data_ab)

  if (nrow (data_ab) < 2) {
    cat("Expression does not generate 2 groups (A/B)\n")
    return(NA)
  }

  # slice A/B groups
  names(data_ab)[1] <- "expression"
  a_grp <- data_ab %>% dplyr::filter(expression == TRUE)
  b_grp <- data_ab %>% dplyr::filter(expression == FALSE)

  # test for significance
  result <- chisq.test(
    matrix(c(
      a_grp$target1_sum, b_grp$target1_sum,
      a_grp$n - a_grp$target1_sum, b_grp$n - b_grp$target1_sum),2,2))

  # result text
  if (result$p.value <= sign_level)  {
    result_txt <- paste0(
      "significant, p value = ", round(result$p.value, 2),
      " (max ", round(sign_level, 2), ")")
    result_lgl <- TRUE
  } else  {
    result_txt <- paste0(
      "not significant, p value = ", round(result$p.value, 2),
      " (max ", round(sign_level, 2), ")")
    result_lgl <- FALSE
  }

  cat(result_txt, "\n")
  result_lgl

} # abtest
