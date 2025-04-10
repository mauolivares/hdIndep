% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max_stat.r
\name{max_stat}
\alias{max_stat}
\title{Calculates the max-test statistic, where the maximum is taken over Chatterjee's rank correlation coefficients.}
\usage{
max_stat(dat)
}
\arguments{
\item{dat}{Data frame. There are two elements in the data frame, X and Y, where X is a random variable and Y corresponds with the \eqn{Y_j} in the individual hypothesis \eqn{Y_j \perp X}.}
}
\value{
Returns the test statistic.
}
\description{
This function returns the statistic \deqn{\hat{T}=\sqrt{n}\max_{1\le j\le p}\hat{\xi}_j}, where \eqn{\hat{\xi}_j} is the Chatterjee's rank correlation coefficient for the \eqn{j}-th hypothesis. See Olivares, Olma, and Wilhelm (2025) for details.
}
\references{
Olivares, M., Olma, T., and Wilhelm, D. (2025). A Powerful Bootstrap Test of Independence in High Dimensions. Preprint, arXiv:2503.21715.
}
\author{
Maurcio Olivares

Tomasz Olma

Daniel Wilhelm
}
\keyword{Chatterjee}
\keyword{Correlation}
\keyword{Rank}
