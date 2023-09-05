# To avoid importing tidyr. here is a simplified tidyr::uncount
# As tidyr::uncount is only used in internal code with the n variable in the data
# We can use a simplified logic.
uncount_compat <- function(dat, wt) {
  rlang::check_required(wt)

  dat <- dplyr::rename(dat, "wt_variable_intermediate" = {{ wt }})
  res <- dplyr::reframe(
    dat,
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~ rep.int(.x, times = .data$wt_variable_intermediate[1])
    ),
    .by = "wt_variable_intermediate"
  )
  res <- dplyr::select(res, -"wt_variable_intermediate")
  res
}
