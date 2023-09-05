% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abtest.R
\name{abtest}
\alias{abtest}
\title{A/B testing}
\usage{
abtest(data, expr, target, sign_level = 0.05)
}
\arguments{
\item{data}{A dataset}

\item{expr}{Logical expression, that return in a FALSE/TRUE}

\item{target}{Target variable}

\item{sign_level}{Significance Level (typical 0.01/0.05/0.10)}
}
\value{
Plot that shows if difference is significant
}
\description{
A/B testing
}
\examples{
## Using chi2-test or t-test depending on target type
data <- create_data_buy(obs = 100)
abtest(data, female_ind == 1, target = buy)  # chi2 test
abtest(data, city_ind == 1, target = age)    # t test

## If small number of observations, Fisher's Exact test
## is used for a binary target (if <= 5 observations in a subgroup)
data <- create_data_buy(obs = 25, seed = 1)
abtest(data, female_ind == 1, target = buy)  # Fisher's Exact test
}