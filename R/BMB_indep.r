#' @title Calculates the block-multiplier bootstrap test.
#'
#' @description This function test \eqn{H_{0,j}: Y_j \perp X}, for \eqn{j=1,\dots,p} using the block-multiplier bootstrap test of See Olivares, Olma, and Wilhelm (2025) for details.
#' @param dat Data frame. There are two elements in the data frame, X and Y, where X is a random variable and Y corresponds with the \eqn{Y_j} in the individual hypothesis \eqn{Y_j \perp X}.
#' @param B Numeric. The number of bootstrap samples.
#' @param alpha Numeric. The significance level.
#' @param type Character. The type of test statistic to be calculated. Options are "bmb1", "bmb2", and "bmb3". See Olivares, Olma, and Wilhelm (2025) for details.
#' @param seed Numeric. The seed for the random number generator. If \code{NULL}, the seed is not set. If a positive integer, it sets the seed for reproducibility.
#' @return An object of class "BMB_indep", a list containing the following components:
#' \item{description}{Type of test, can be Difference of Means, Medians, or Variances.}
#' \item{n}{Sample Size.}
#' \item{p}{Number of hypotheses.}
#' \item{block_size}{Block size used in the bootstrap.}
#' \item{B}{Number of bootstrap samples.}
#' \item{T_obs}{Test statistic. It can be one of three options: BMB1, BMB2, or BMB3. See Olivares, Olma, and Wilhelm (2025) for details.}
#' \item{type}{Type of test statistic; see above.}
#' \item{cv}{Bootstrap critical value corresponding to the test statistic.}
#' @author Maurcio Olivares
#' @author Tomasz Olma
#' @author Daniel Wilhelm
#' @references
#' Olivares, M., Olma, T., and Wilhelm, D. (2025). A Powerful Bootstrap Test of Independence in High Dimensions. Preprint, arXiv:2503.21715.
#' @keywords Chatterjee Rank Correlation
#' @include BMB.cv.r max_stat.r block_size.r
#' @importFrom compiler cmpfun
#' @examples
#' \dontrun{
#' n <- 100
#' p <- 10
#' dat <- list()
#' dat$X <- rnorm(n)
#' dat$Y <- MASS::mvrnorm(n = n, mu = rep(0,  p), Sigma=diag(rep(1, p)))
#' BMB_indep(dat, B=100, alpha=0.05, type = "bmb1", seed = 5)
#'
#' }
#' @export
#'
BMB_indep <- function(dat) {


}
BMB_indep <- compiler::cmpfun(BMB_indep)