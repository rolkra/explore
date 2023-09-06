# To avoid importing tidyr. here is a simplified tidyr::uncount
# As tidyr::uncount is only used in internal code with the n variable in the data
# We can use a simplified logic.
uncount_compat <- function(dat, wt) {
  rlang::check_required(wt)

  res <- dplyr::reframe(
    dat,
    var_wt = rep.int(1, times = sum({{ wt }})),
    .by = !{{ wt }}
  )
  res <- dplyr::select(res, -"var_wt")
  res
}
