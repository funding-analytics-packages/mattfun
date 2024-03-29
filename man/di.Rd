% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/di.r
\name{di}
\alias{di}
\title{Display summary statistics}
\usage{
di(df, vars = colnames(df))
}
\arguments{
\item{df}{object (i.e. data.frame) to display properties of}

\item{vars}{vector of strings to indicate which columns to include in the
output. By default all columns are included}
}
\description{
\code{di} loads \code{View} of summary statistics about the dataset \code{df}
}
\details{
This function displays summary statistics about \code{df} to help analyse
  the state of the data. It lists number of unique values, number of NAs, etc.
}
\examples{
data = data.frame(apple = 1:4,
                  orange = letters[1:4],
                  orangutan = c("dfg", "sdf", NA, "134"),
                  applause = runif(4),
                  a = c(TRUE, FALSE, FALSE, NA))

di(data)
}
