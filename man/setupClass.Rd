% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setupClass.R
\name{setupClass}
\alias{setupClass}
\title{setupClass function}
\usage{
setupClass(
  data,
  betas,
  time = FALSE,
  ind = FALSE,
  time_col = "time",
  ind_col = "ind"
)
}
\arguments{
\item{data}{matrix or ExpressionSet object}

\item{betas}{named vector of beta values}

\item{time}{vector of follow up time values (if not in ExprSet), or FALSE to ignore}

\item{ind}{vector of event indicators (0 = no event/1 = event; if not in ExprSet), or FALSE to ignore}

\item{time_col}{character object giving the column name for the follow up time}

\item{ind_col}{character object giving the column name for the event indicator}
}
\description{
setupClass function
}
\examples{
## If you have your ExprSet with columns called time and ind

setupClass(ExprSet, betas)

## If you have your ExprSet with columns named differently

setupClass(ExprSet,
        betas,
        time_col = "PROG_TIME",
        ind_col = "PROG_IND")

## If you have an expression matrix, beta values and seperate time and ind vectors
## Make sure the vectors and the matrix are all in the correct orders.

setupClass(data = matrix,
        betas,
        time = time_vector,
        ind = ind_vector,
        time_col = "PROG_TIME",
        ind_col = "PROG_IND")

}
