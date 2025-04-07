#' @title Calculates the block-multiplier bootstrap test.
#'
#' @description This function test \eqn{H_{0,j}: Y_j \perp X}, for \eqn{j=1,\dots,p} using the block-multiplier bootstrap test of See Olivares, Olma, and Wilhelm (2025) for details.
#' @param dat Data frame. There are two elements in the data frame, X and Y, where X is a random variable and Y corresponds with the \eqn{Y_j} in the individual hypothesis \eqn{Y_j \perp X}.
#' @param B Numeric. The number of bootstrap samples.
#' @param alpha Numeric. The significance level.
#' @param type Character. The type of test statistic to be calculated. Options are "bmb", "bmb1", and "bmb2", which correspond to \eqn{\hat{T}^B}, \eqn{\hat{T}^{B,stud1}}, and \eqn{\hat{T}^{B, stud2}}, respectively. See Olivares, Olma, and Wilhelm (2025) for details.
#' @param seed Numeric. The seed for the random number generator. If \code{NULL}, the seed is not set. If a positive integer, it sets the seed for reproducibility.
#' @return An object of class "BMB_indep", a list containing the following components:
#' \item{type{Type of test Statistics.}
#' \item{T_obs}{Test statistic. It can be one of three options: BMB, BMB1, or BMB2. See Olivares, Olma, and Wilhelm (2025) for details.}
#' \item{n}{Sample Size.}
#' \item{p}{Number of hypotheses.}
#' \item{block_size}{Block size used in the bootstrap.}
#' \item{B}{Number of bootstrap samples.}
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
#' test_indep <- BMB_indep(dat, B=100, alpha=0.05, type = "bmb1", seed = 5)
#' summary(test_indep)
#'
#' }
#' @export
#'
BMB_indep <- function(dat, B, alpha, type = c("bmb", "bmb1", "bmb2"), seed = NULL) {
  # Check if seed is NULL or a positive integer
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || seed <= 0 || seed != round(seed))) {
    stop("Seed must be a positive integer or NULL.")
  }
  type <- match.arg(type)
  # Calculate the sample size
  n <- length(dat$X)
  # Calculate the optimal block size
  q <- block_size(n)
  # Calculate the test statistic
  T_obs <- max_stat(dat)
  # Calculate the critical value
  cv <- BMB.cv(dat, q, B, alpha, type, seed)$cv
  # Decision rule
  if (T_obs > cv) {
    decision <- "Reject the null hypothesis"
  } else {
    decision <- "Do not reject the null hypothesis"
}

  object_hdindep<-list() #Generates an empty list to collect all the required info for summary
  object_hdindep$type <- type
  object_hdindep$T_obs <- T_obs
  object_hdindep$n <- n
  object_hdindep$p <- ncol(dat$Y)
  object_hdindep$block_size <- q
  object_hdindep$B <- B
  object_hdindep$cv <- cv
  object_hdindep$alpha <- alpha
  object_hdindep$decision <- decision
  class(object_hdindep) <- "BMB_indep"

  # Returns the object
  object_hdindep

}
BMB_indep <- compiler::cmpfun(BMB_indep)

