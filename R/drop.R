#' Drop all variables with no variance
#'
#' @param data Data frame
#' @return Data frame
#' @examples
#' data <- data.frame(a = 1:10, b = rep(1,10))
#' drop_var_no_variance(data)
#' @export

drop_var_no_variance <- function(data) {
  out <- lapply(data, function(x) length(unique(x)))
  want <- which(!out > 1)
  dplyr::select(data, -all_of(names(data)[want]))
}

#' Drop all not numeric variables
#'
#' @param data Data frame
#' @return Data frame
#' @examples
#' data <- data.frame(a = 1:10, b = rep("A",10))
#' drop_var_not_numeric(data)
#' @export

drop_var_not_numeric <- function(data) {
  dplyr::select(data, dplyr::where(is.numeric))
}

#' Drop all variables with NA-values
#'
#' @param data Data frame
#' @return Data frame
#' @examples
#' data <- data.frame(a = 1:10, b = rep(NA,10))
#' drop_var_with_na(data)
#' @export

drop_var_with_na <- function(data) {
  sel <- colSums(is.na(data))==0
  dplyr::select(data, which(sel))
}

#' Drop all observations with NA-values
#'
#' @param data Data frame
#' @return Data frame
#' @examples
#' data <- data.frame(a = 1:10, b = rep("A",10))
#' data[1,1] <- NA
#' drop_obs_with_na(data)
#' @export

drop_obs_with_na <- function(data) {
  result <- data[rowSums(is.na(data))==0 , ]
  if (!is.data.frame(result)) {
    result <- data.frame()
  }
  result
}

#' Drop all observations where expression is true
#'
#' @param data Data frame
#' @param expr Expression
#' @return Data frame
#' @examples
#' drop_obs_if(iris, Species == "setosa")
#' drop_obs_if(iris, Sepal.Length < 5 | Sepal.Length >7)
#' @export

drop_obs_if <- function(data, expr) {
  dplyr::filter(data, !{{ expr }})
}

#' Check vector for low variance
#'
#' @param values Vector of values
#' @param max_prop Maximum proportion of values without variance
#' @return TRUE/FALSE (low variance)
#' @examples
#' \dontrun{
#' values <- c(1, rep(0 ,1000))
#' check_vec_low_variance(values, max_prop = 0.9)
#' }

check_vec_low_variance <- function(values, max_prop = 0.99) {

  # frequency of values
  t <- table(values)

  # check most frequent value
  m <- max(t)
  if (m >= sum(t) * max_prop) {
    result <- TRUE
  } else {
    result <- FALSE
  }

  # return result
  result
} # check_vec_low_variance()


#' Drop all variables with low variance
#'
#' @param data Data frame
#' @param max_prop Maximum proportion of values without variance
#' @return Data frame
#' @examples
#' data <- data.frame(a = 1:100, b = c(0, rep(1, 99)))
#' drop_var_low_variance(data, max_prop = 0.9)
#' @export

drop_var_low_variance <- function(data, max_prop = 0.99) {
  check <- apply(data, 2, check_vec_low_variance, max_prop = max_prop)
  data[!check]
}

#' Drop variables by name
#'
#' @param data Data frame
#' @param var_names Vector of variable names (as string)
#' @return Data frame
#' @examples
#' drop_var_by_names(iris, "Species")
#' drop_var_by_names(iris, c("Sepal.Length", "Sepal.Width"))
#' @export

drop_var_by_names <- function(data, var_names) {
  check <- var_names %in% names(data)
  var_names_exist <- var_names[check]
  var_names_drop <- names(data) %in% var_names_exist
  data[!var_names_drop]
}
