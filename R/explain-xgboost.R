#' Drop all non numeric variables
#'
#' @param data Data frame
#' @return Data frame

drop_var_non_numeric <- function(data) {
  dplyr::select(data, dplyr::where(is.numeric))
}

#' Drop all variables with NA-values
#'
#' @param data Data frame
#' @return Data frame

drop_var_with_na <- function(data) {
  data[ , colSums(is.na(data))==0]
}

#' Drop all observations with NA-values
#'
#' @param data Data frame
#' @return Data frame

drop_obs_with_na <- function(data) {
  data[rowSums(is.na(data))==0 , ]
}


#' Log conditional
#'
#' @param log log (TRUE|FALSE)
#' @param text text string to be logged
#' @return prints log on screen (if log == TRUE).

log_info_if <- function(log = TRUE, text = "log") {
  if (log) {message(text)}
}

#' Explain xgboost
#'
#' Based on the hyperparameters defined in the config.yml, XGBoost hyperparameter-tuning is
#' carried out using cross-validation. The best model is chosen and returned.
#' If defined/necessary, downsampling will be performed as part of the hyperparameter-tuning.
#' The results of the hyperparameter will be plotted and exported as a dataframe.
#'
#' @param data Data frame, must contain variable defined in target,
#' but should not contain any customer-IDs or date/period columns
#' @param target Target variable (must be binary 0/1, FALSE/TRUE, no/yes)
#' @param log Log?
#' @param setup Setup of model
#' @param out Output of the function: "plot" | "model" | "importance" | all"
#'
#' @return model as list
#' @export

explain_xgboost <- function(data, target, log = TRUE,
                               setup = list(
                                 cv_nfold = 2, # Nr. of folds used for cross-validation during model training
                                 max_nrounds = 1000,
                                 early_stopping_rounds = 50,
                                 grid_xgboost = list(
                                   eta = c(0.3, 0.1, 0.01),
                                   max_depth = c(3, 5),
                                   gamma = 0,
                                   colsample_bytree = 0.8,
                                   subsample = 0.8,
                                   min_child_weight = 1,
                                   scale_pos_weight = 1
                               )),
                            out = "plot") {

  # check if xgboost is installed
  rlang::check_installed("xgboost", reason = "to create a xgboost model.")

  # chech data & target
  check_data_frame_non_empty(data)
  rlang::check_required(target)

  # tidy eval for target
  target_quo <- enquo(target)
  target_txt <- quo_name(target_quo)[[1]]
  if (!target_txt %in% names(data)) {
    warning("target must be a variable of data")
    return(invisible())
  }

  # check if target is binary
  if (length(unique(data[[target_txt]])) != 2)  {
    warning("target must be binary (e.g. 0/1, TRUE/FALSE, 'yes'/'no')")
    return(invisible())
  }


  # undefined variables to check CRAN tests
  variable <- NULL

  # define hy-param grid
  param_grid <- expand.grid(
    #min_prop_train = setup$min_prop_train,
    eta = setup$grid_xgboost$eta,
    max_depth = setup$grid_xgboost$max_depth,
    gamma = setup$grid_xgboost$gamma,
    colsample_bytree = setup$grid_xgboost$colsample_bytree,
    subsample = setup$grid_xgboost$subsample,
    min_child_weight = setup$grid_xgboost$min_child_weight,
    scale_pos_weight = setup$grid_xgboost$scale_pos_weight
  )

  # log details?
  verbose <- FALSE

  # prepare to remember
  all_auc <- vector(mode = "numeric")
  all_nrounds <- vector(mode = "numeric")
  hp_tuning_log <- NULL


  # train with cross validation ---------------------------------------------

  # xgb.cv loop
  k <- 1
  for (k in seq_len(nrow(param_grid))) {

    current_params <- setNames(
      as.list(t(param_grid[k, ])),
      names(param_grid)
    )

    # prepare target
    dtrain <- xgboost::xgb.DMatrix(
      as.matrix(data[ ,names(data) != target_txt]),
      label = data[[target_txt]])

    t1 <- Sys.time()

    log_info_if(log, paste("\ntrain xgboost nr", k))
    log_str <- paste0(paste0(names(current_params), "=", current_params), collapse=", ")
    log_info_if(log, paste0("", log_str))

    # training
    set.seed(42)
    cv <- xgboost::xgb.cv(
      objective = "binary:logistic",
      eval_metric = "auc",
      data = dtrain,
      #label = ltrain,
      params = current_params,
      nthread = 1, #setup$nthread,
      nfold = setup$cv_nfold,
      nrounds = setup$max_nrounds,
      early_stopping_rounds = setup$early_stopping_rounds,
      maximize = TRUE,
      verbose = FALSE, #verbose,     #log details?
      print_every_n = 100
    )
    #log_info_if(log, paste("xgboost nr", k, "training finished"))
    all_nrounds[k] <- cv$best_iteration
    model_log <- cv$evaluation_log[cv$evaluation_log$iter == cv$best_iteration, ]
    all_auc[k] <- model_log$test_auc_mean

    t2 <- Sys.time()
    runtime_curr <- round(difftime(t2, t1, units = "mins"), 1)

    #store test_auc for each xgb iter
    hp_tuning_log_curr <- cv$evaluation_log %>%
      dplyr::select(iter, train_auc_mean, test_auc_mean) %>%
      dplyr::mutate(model_nr = k,
                    min_prop_train = current_params$min_prop_train,
                    eta = current_params$eta,
                    gamma = current_params$gamma,
                    max_depth = current_params$max_depth,
                    min_child_weight = current_params$min_child_weight,
                    subsample = current_params$subsample,
                    colsample_bytree = current_params$colsample_bytree,
                    scale_pos_weight = current_params$scale_pos_weight,
                    best_iter_ind = ifelse(iter == cv$best_iteration, 1, 0),
                    runtime = runtime_curr,
                    .before = iter)
    hp_tuning_log <- rbind(hp_tuning_log, hp_tuning_log_curr)
    rm(hp_tuning_log_curr)

    log_info_if(log, paste0("",
      all_nrounds[k], " iterations, ",
      "training time=", runtime_curr, " min, ",
      "auc=", round(all_auc[k],4)))

  } #end xgb.cv loop

  # Plot & Save Hy-Param Tuning Runs
  suppressWarnings({
    suppressMessages({
      plot_hp_tuning <- hp_tuning_log %>%  #nolint
        dplyr::mutate(model_nr = as.factor(model_nr)) %>%
        ggplot2::ggplot(ggplot2::aes(x = iter, y = test_auc_mean, color = model_nr)) +
        ggplot2::geom_line(size = 1.5) +
        ggplot2::labs(x = "XGBoost Training Iteration", y = "Mean Test AUC (CV)",
                      title = "Hyperparameter-Tuning") +
        ggplot2::theme_light()
    })
  })

  # Store Hy-Param Tuning Run Results as df
  hp_tuning_best <- hp_tuning_log %>%
    dplyr::filter(best_iter_ind == 1)

  # identify cols w/o variance for removal
  drop <- hp_tuning_best %>%
    dplyr::select(-runtime, -iter) %>%  #should not be dropped, even if constant across all runs
    explore::describe() %>%
    dplyr::filter(unique == 1) %>%
    dplyr::pull(variable)

  # remove constant hy-params from result df
  hp_tuning_best <- hp_tuning_best %>%
    dplyr::select(-dplyr::one_of(drop))


# final model selection & training ----------------------------------------

  # Select best params
  best_idx <- which(all_auc == max(all_auc))[1]
  best_auc <- all_auc[best_idx]
  best_nrounds <- all_nrounds[best_idx]
  best_params <- setNames(
    as.list(t(param_grid[best_idx, ])),
    names(param_grid)
  )

  log_info_if(log, paste0("\nbest model found in cross-validation: xgboost nr ", best_idx, ": "))
  log_info_if(log, paste0("", paste0(paste0(names(current_params), "=", current_params), collapse=", ")))
  log_info_if(log, paste0("nrounds=", best_nrounds,", test_auc=", round(best_auc, 4)))

  log_info_if(log, paste0("\ntrain final model..."))

  # train final model, no cross validation
  t1 <- Sys.time()
  model <- xgboost::xgb.train(
    data = dtrain,
    nrounds = best_nrounds,
    nthread = 1, #setup$nthread,
    booster = "gbtree",
    objective = "binary:logistic",
    params = best_params,
    verbose = verbose,
    print_every_n = 100
  )
  t2 <- Sys.time()
  runtime_curr <- round(difftime(t2, t1, units = "mins"), 1)

  # feature importance
  importance = xgboost::xgb.importance(colnames(dtrain), model = model)
  names(importance) <- c("variable", "gain", "cover", "frequency")
  importance$importance <- importance$gain
  importance <- importance %>% dplyr::arrange(dplyr::desc(importance))

  # plot importance
  p <- importance %>%
    utils::head(30) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = importance,
      y = forcats::fct_reorder(variable, importance)
    )) +
    ggplot2::geom_col(color = "white", fill = "grey") +
    ggplot2::ylab("variable") +
    ggplot2::ggtitle("ML-feature-importance") +
    ggplot2::theme_minimal()


  # log
  log_info_if(log, paste0("done, ", "training time=", runtime_curr, " min"))


  # return result -----------------------------------------------------------

  model <- list(
    model = model,
    importance = importance,
    plot = p,
    tune_data = hp_tuning_best,
    tune_plot = plot_hp_tuning
  )

  # output
  if (out %in% c("all", "list")) {
    return(model)
  } else if(out == "model") {
    return(model$model)
  } else if(out == "importance") {
    return(model$importance)
  }

  # default output
  model$plot

} # explain_xgboost
