% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-var.R
\name{add_var_random_moon}
\alias{add_var_random_moon}
\title{Add a random moon variable to dataset}
\usage{
add_var_random_moon(data, name = "random_moon", overwrite = TRUE, seed)
}
\arguments{
\item{data}{A dataset}

\item{name}{Name of new variable (as string)}

\item{overwrite}{Can new random variable overwrite an existing variable in dataset?}

\item{seed}{Seed for random number generation (integer)}
}
\value{
Dataset containing new random variable
}
\description{
Add a random moon variable to dataset
}
\examples{
library(magrittr)
iris \%>\% add_var_random_moon() \%>\% head()
}
