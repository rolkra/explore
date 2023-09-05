check_data_frame_non_empty <- function(data, error_call = rlang::caller_env()) {
  check_data_frame(data, call = error_call)
  check_number_whole(nrow(data), min = 1, call = error_call)
}
