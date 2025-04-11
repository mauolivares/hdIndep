#' @title Calculates the block-multiplier bootstrap test.
#'
#' @description This function test \eqn{H_{0,j}: Y_j \perp X}, for \eqn{j=1,\dots,p} using the block-multiplier bootstrap test of Olivares, Olma, and Wilhelm (2025).
#' @param dat Data frame. There are two elements in the data frame, X and Y, where X is a random variable and Y corresponds with the \eqn{Y_j} in the individual hypothesis \eqn{Y_j \perp X}.
#' @param B Numeric. The number of bootstrap samples.
#' @param alpha Numeric. The significance level.
#' @param type Character. This argument specifies whether and how the test statistic and the bootstrap statistic are studentized. Options are "bmb" (no studentization), "bmb1" (default option), and "bmb2" (alternative studentization). The types are formally described in Olivares, Olma, and Wilhelm (2025).
#' @param seed Numeric. The seed for the random number generator. If \code{NULL}, the seed is not set. If a positive integer, it sets the seed for reproducibility.
#' @return An object of class "BMB_indep", a list containing the following components:
#' \item{type}{Type. See description above}
#' \item{T_obs}{The value of the test statistic.}
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
#' test_indep <- BMB_indep(dat, B=100, alpha=0.05, type = "bmb", seed = 5)
#' summary(test_indep)
#'
#' }
#' @export
#'
BMB_indep <- function(dat, B, alpha, type = "bmb1", seed = NULL) {
  # Check if seed is NULL or a positive integer
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || seed <= 0 || seed != round(seed))) {
    stop("Seed must be a positive integer or NULL.")
  }
  if (!is.null(seed)) set.seed(seed) # set the seed if provided

  # Match the type while allowing for case-insensitivity matching
  type <- match.arg(tolower(type), choices = c("bmb", "bmb1", "bmb2"))
  # Check if the type is vald
  if (!type %in% c("bmb", "bmb1", "bmb2")) {
    stop("Invalid type. Choose one of 'bmb', 'bmb1', or 'bmb2'.")
  }
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
  #Generate an empty list to collect all the required info for summary
  object_hdindep <- list()
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

