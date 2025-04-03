#' @title Calculates the max-test statistic, where the maximum is taken over Chatterjee's rank correlation coefficients.
#'
#' @description This function returns the statistic \deqn{\hat{T}=\sqrt{n}\max_{1\le j\le p}\hat{\xi}_j}, where \eqn{\hat{\xi}_j} is the Chatterjee's rank correlation coefficient for the \eqn{j}-th hypothesis. See Olivares, Olma, and Wilhelm (2025) for details.
#' @param dat Data frame. There are two elements in the data frame, X and Y, where X is a random variable and Y corresponds with the \eqn{Y_j} in the individual hypothesis \eqn{Y_j \perp X}.
#' @return Returns the test statistic.
#' @author Maurcio Olivares
#' @author Tomasz Olma
#' @author Daniel Wilhelm
#' @references
#' Olivares, M., Olma, T., and Wilhelm, D. (2025). A Powerful Bootstrap Test of Independence in High Dimensions. Preprint, arXiv:2503.21715.
#' @keywords Chatterjee Rank Correlation
#' @import compiler
#' @export


max.stat <- function(dat) {

  n <- length(dat$X)
  PI <- rank(dat$X, ties.method = "random")
  ord <- order(PI)

  fr <- as.matrix(apply(as.matrix(dat$Y), MARGIN=2, function(yvec) rank(yvec, ties.method = "max")/n))
  gr <- as.matrix(apply(as.matrix(dat$Y), MARGIN=2, function(yvec) rank((-yvec), ties.method = "max")/n))

  fr <- fr[ord, , drop=FALSE]
  A1 <- colSums(abs(fr[1:(n - 1), , drop = FALSE] - fr[2:n, , drop = FALSE])) / (2 * n)
  CU <- colMeans(gr * (1 - gr))

  xis <- 1 - A1 / CU
  # Return the test statistic
  sqrt(n) * max(xis)
}
max.stat <- cmpfun(max.stat)