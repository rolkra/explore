% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explain-xgboost.R
\name{explain_xgboost}
\alias{explain_xgboost}
\title{Explain xgboost}
\usage{
explain_xgboost(
  data,
  log = TRUE,
  log_details = FALSE,
  setup = list(cv_nfold = 2, max_nrounds = 1000, early_stopping_rounds = 50, grid_xgboost
    = list(eta = c(0.3, 0.1, 0.01), max_depth = c(3, 5), gamma = 0, colsample_bytree =
    0.8, subsample = 0.8, min_child_weight = 1, scale_pos_weight = 1))
)
}
\arguments{
\item{data}{data frame, must contain variable target_ind,
but should not contain any customer-IDs or date/period columns}

\item{log}{Log?}

\item{log_details}{If set to FALSE (default), no logging of xgb.cv}

\item{setup}{Setup of model}
}
\value{
model as list
}
\description{
Based on the hyperparameters defined in the config.yml, XGBoost hyperparameter-tuning is
carried out using cross-validation. The best model is chosen and returned.
If defined/necessary, downsampling will be performed as part of the hyperparameter-tuning.
The results of the hyperparameter will be plotted and exported as a dataframe.
}