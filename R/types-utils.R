check_data_frame_non_empty <- function(x, error_call = rlang::caller_env()) {
  check_data_frame(x, call = error_call)
  check_number_whole(nrow(x), min = 1, call = error_call)
}
