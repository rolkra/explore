#' Explain a target using a simple decision tree (classification or regression)
#'
#' @param data A dataset
#' @param target Target variable
#' @param n weights variable (for count data)
#' @param max_cat Drop categorical variables with higher number of levels
#' @param max_target_cat Maximum number of categories to be plotted for target (except NA)
#' @inheritParams rpart::rpart.control
#' @inheritParams rpart::rpart
#' @param size Text size of plot
#' @param out Output of function: "plot" | "model"
#' @param ... Further arguments
#' @return Plot or additional the model (if out = "model")
#' @examples
#' data <- iris
#' data$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
#' data$Species <- NULL
#' explain_tree(data, target = is_versicolor)
#' @export

explain_tree <- function(data, target, n,
                         max_cat = 10, max_target_cat = 5, maxdepth = 3,
                         minsplit = 20, cp = 0, weights = NA,
                         size = 0.7, out = "plot", ...)  {
  check_data_frame_non_empty(data)
  # define variables to pass CRAN-checks
  type <- NULL
  variable <- NULL

  # parameter n, uncount
  if (!missing(n))  {
    n_quo <- enquo(n)
    n_txt <- quo_name(n_quo)[[1]]
    # convert all character to factor
    data[sapply(data, is.character)] <- lapply(
      data[sapply(data, is.character)], as.factor)
    # uncount data
    data <- uncount_compat(data, wt = !!n_quo)
  } else {
    n_txt = NA
  }

  # parameter target
  if (!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
    return(NULL)
  }

  # no obs-weights for count-data
  if (!missing(n)) {
    weights <- NA
  }

  # default obs-weights
  if (!is.vector(weights) | length(weights) < nrow(data)) {
    weights <- rep(1, nrow(data))
  }

  # parameter minsplit
  if(is.na(minsplit)) {
    minsplit <- sum(weights)/10
  }

  # drop variables, that are not usable
  d <- describe(data)
  var_keep <- d %>%
    filter(type %in% c("lgl", "int", "dbl", "chr", "fct")) %>%
    filter(type != "chr" | (type == "chr" & unique <= max_cat) | variable == target_txt) %>%
    pull(variable)
  data <- data %>% select(one_of(as.character(var_keep)))

  # minimum 2 variables left?
  if (ncol(data) < 2) {
    p <- plot_text("can't grow decision tree")
    return(invisible(p))
  }

  # observations?
  if(nrow(data) == 0) {
    p <- plot_text("can't grow decision tree")
    return(invisible(p))
  }

  # target all NA?
  if (all(is.na(data[[target_txt]])))  {
    p <- plot_text("can't grow decision tree")
    return(invisible(p))
  }

  # convert target into formula
  formula_txt <- as.formula(paste(target_txt, "~ ."))

  if (guess_cat_num(data[[target_txt]]) == "cat")  {

    # convert target to factor if necessary
    if (!is.factor(data[[target_txt]]))  {
      data[[target_txt]] <- factor(data[[target_txt]])
      data[[target_txt]] <- forcats::fct_lump_n(data[[target_txt]], n =max_target_cat, other_level = ".OTHER")
    }

    # get prior probabilities
    n <- table(data[[target_txt]])
    prior <- n / sum(n)


    if (all(weights == 1)) {

      # create tree cat with prior probabilities
      mod <- rpart::rpart(formula_txt,
                          data = data,
                          method = "class",
                          parms = list(prior = prior),
                          control = rpart::rpart.control(maxdepth=maxdepth, minsplit=minsplit, cp=cp))
    } else {

      # create tree cat with weights
      mod <- rpart::rpart(formula_txt,
                          data = data,
                          method = "class",
                          weights = weights,
                          control = rpart::rpart.control(maxdepth=maxdepth, minsplit=minsplit, cp=cp))


    } # checking weights

  } else {

    # create tree num
    mod <- rpart::rpart(formula_txt,
                        data = data,
                        weights = weights,
                        method = "anova",  #"class",
                        control = rpart::rpart.control(maxdepth=maxdepth, minsplit=minsplit, cp=cp))

  } # if

  # check if tree was created. If not just plot info-text
  if(nrow(mod$frame) > 1)  {

    # plot tree
    rpart.plot::rpart.plot(mod,
                           prefix="target = ",       # prefix text in first line in node
                           type=2,                   # 2: split variable name under box, 5: split variable name in the interior nodes
                           yesno=2,                  # show yes/no at each node
                           #extra=107,                # 106 = % observations + target
                           branch=0,                 # 0 = V shaped, 1 = squared
                           branch.type=5,            # 5 = proportional width
                           box.palette = 'Blues',    # colors for nodes
                           shadow.col = 0,           # color of shadow, 0 = none
                           cex = size,
                           left = FALSE,
                           ...)
  } else {

    plot_text("can't grow decision tree")
  } # if tree exists

  # what to return
  if(out == "model") {
    return(mod)
  }

} # explain_tree

#' Explain a binary target using a logistic regression (glm).
#' Model chosen by AIC in a Stepwise Algorithm (`MASS::stepAIC()`).
#'
#' @param data A dataset
#' @param target Target variable (binary)
#' @param out Output of the function: "tibble" | "model"
#' @param ... Further arguments
#' @return Dataset with results (term, estimate, std.error, z.value, p.value)
#' @examplesIf rlang::is_installed("MASS")
#' data <- iris
#' data$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
#' data$Species <- NULL
#' explain_logreg(data, target = is_versicolor)
#' @export

explain_logreg <- function(data, target, out = "tibble", ...)  {

  rlang::check_installed("MASS", reason = "to create a model with AIC.")
  check_data_frame_non_empty(data)
  # parameter data

  # parameter target
  if (!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
  }

  if (sum(complete.cases(data)) < nrow(data))  {
    warning("can't calculate logreg, drop rows with NA first")
    return()
  }

  if (length(unique(data[[target_txt]])) != 2)  {
    warning("target must be binary (e.g. 0/1, TRUE/FALSE, 'yes'/'no')")
    return(invisible())
  }

  # convert target into formula
  formula_txt <- as.formula(paste(target_txt, "~ ."))

  mod <- suppressWarnings(stats::glm(formula_txt, data = data, family = "binomial"))
  mod_stepwise <- suppressWarnings(MASS::stepAIC(mod, trace = FALSE))

  mod_step_summary <- suppressMessages(summary(mod_stepwise))

  df_model_raw <- tibble::as_tibble(mod_step_summary$coefficients, rownames = "term")
  names(df_model_raw) <- c("term", "estimate", "std.error", "statistic", "p.value")
  df_model <- df_model_raw
  # output
  if (out == "tibble") {
    df_model
  } else {
    mod_stepwise
  }


} # explain_logreg


#' Explain a target using Random Forest.
#'
#' @param data A dataset
#' @param target Target variable (binary)
#' @param ntree Number of trees used for Random Forest
#' @param out Output of the function: "plot" | "model" | "importance" | all"
#' @param ... Further arguments
#' @return Plot of importance (if out = "plot")
#' @examplesIf rlang::is_installed("randomForest")
#' data <- create_data_buy()
#' explain_forest(data, target = buy)
#' @export

explain_forest <- function(data, target, ntree = 50, out = "plot", ...)  {
  rlang::check_installed("randomForest", reason = "to create a random forest model.")
  # undefined variables to pass CRAN check
  variable <- NULL

  # parameter data
  check_data_frame_non_empty(data)
  rlang::check_required(target)
  # parameter target
  target_quo <- enquo(target)
  target_txt <- quo_name(target_quo)[[1]]

  # classification or regression
  # use factor for classification
  target_values <- data[[target_txt]]
  guess <- guess_cat_num(target_values)
  if (guess == "cat" && !is.factor(target_values)) {
    data[[target_txt]] <- as.factor(target_values)
  }

  # train random forest
  formula_txt <- as.formula(paste(target_txt, "~ ."))
  rf <- randomForest::randomForest(formula_txt, data=data, ntree = ntree, ...)

  # minimize memory usage
  rf$votes <- NULL
  rf$predicted <- NULL
  rf$oob.times <- NULL
  rf$y <- NULL

  # convert importance into data frame
  importance <- as.data.frame(rf$importance)
  importance$variable <- row.names(importance)
  importance$importance <- importance[[1]]
  importance$MeanDecreaseGini <- NULL           # used if classification
  importance$IncNodePurity <- NULL              # used if regression
  importance <- importance %>% dplyr::arrange(dplyr::desc(importance))

  # plot importance
  p <- importance %>%
    utils::head(30) %>%
    ggplot2::ggplot(aes(
      x = importance,
      y = forcats::fct_reorder(variable, importance)
      )) +
    ggplot2::geom_col(color = "white", fill = "grey") +
    ggplot2::ylab("variable") +
    ggplot2::ggtitle("ML-feature-importance") +
    ggplot2::theme_minimal()

  # output
  model <- list(rf = rf, importance = importance, plot = p)

  # output
  if (out %in% c("all", "list")) {
    return(model)
  } else if(out == "model") {
    return(rf)
  } else if(out == "importance") {
    return(importance)
  }

  # default output
  p

} # explain_forest


#' Predict target using a trained model.
#'
#' @param data A dataset (data.frame or tbl)
#' @param model A model created with `explain_*()` function
#' @param name Prefix of variable-name for prediction
#' @return data containing predicted probabilities for target values
#' @examplesIf rlang::is_installed(c("rpart", "randomForest", "xgboost"))
#' data_train <- create_data_buy(seed = 1)
#' data_test <- create_data_buy(seed = 2)
#' model <- explain_tree(data_train, target = buy, out = "model")
#' data <- predict_target(data = data_test, model = model)
#' describe(data)
#' @export

predict_target <- function(data, model, name = "prediction") {

  # check parameter
  check_data_frame_non_empty(data)

  result <- data
  values <- NA

  if (inherits(model, "randomForest") && model$type == "classification") {

    values <- stats::predict(model, newdata = data, type = "prob")
    var_names <- paste0(name, "_", colnames(values))

  } else if (inherits(model, "randomForest") && model$type == "regression") {

    values <- stats::predict(model, newdata = data)
    var_names <- paste0(name)

  } else if (inherits(model, "glm")) {

    values <- stats::predict(model, newdata = data, type = "response")
    var_names <- paste0(name)

  } else if (inherits(model, "rpart") && model$method == "class") {

    values <- stats::predict(model, newdata = data, type = "prob")
    var_names <- paste0(name, "_", colnames(values))

  } else if (inherits(model, "rpart") && model$method == "anova") {

    values <- stats::predict(model, newdata = data)
    var_names <- paste0(name)

  } else if (inherits(model, "xgb.Booster")) {

    if (utils::packageVersion("xgboost") < "3.0.0.0") {

      values <- stats::predict(
        model,
        newdata = as.matrix(data[ ,model$feature_names]),
        #newdata = as.matrix(data[ , c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]),
        type = "prob")
      var_names <- paste0(name)

    } else {

      values <- stats::predict(
        model,
        #newdata = as.matrix(data[ ,model$feature_names]),
        newdata = as.matrix(data),
        type = "prob",
        validate_features = TRUE)
      var_names <- paste0(name)

    }

  }

  if (anyNA(values)) {
    warning("Predicting target not possible")
    return(result)
  }

  # add predicted target values to data
  var_names <- stringr::str_trim(var_names) %>%
    stringr::str_replace_all("[ ]", "_")

  df <- as.data.frame(values)
  names(df) <- var_names
  result <- cbind(data, df)

  # return data
  result
}
