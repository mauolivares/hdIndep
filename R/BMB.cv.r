#' @title Calculates the Gaussian BMB critical value based on the max-test statistic, where the maximum is taken over Chatterjee's correlation coefficients.
#'
#' @description This function returns the bootstrap critical value of the statistic \deqn{T^B=\max_{1\le j\le p}\frac{1}{\sqrt{mq}}\sum_{k=1}^{m} \hat{A}_{j,k} \cdot \varepsilon_{k}} based on \eqn{B} block multiplier bootstrap samples.
#' @param dat Data frame. There are two elements in the data frame, X and Y, where X is a random variable and Y corresponds with the \eqn{Y_j} in the individual hypothesis \eqn{Y_j \perp X}.
#' @param q Numeric. The block size.
#' @param B Numeric. The number of bootstrap samples.
#' @param alpha Numeric. The significance level.
#' @param type Character. This argument specifies whether and how the test statistic and the bootstrap statistic are studentized. Options are "bmb" (no studentization), "bmb1" (default option), and "bmb2" (alternative studentization). The types are formally described in Olivares, Olma, and Wilhelm (2025).
#' @param seed Numeric. The seed for the random number generator. If \code{NULL}, the seed is not set. If a positive integer, it sets the seed for reproducibility.
#' @return Returns a list with the critical value and the type of test statistic.
#' @author Maurcio Olivares
#' @author Tomasz Olma
#' @author Daniel Wilhelm
#' @references
#' Olivares, M., Olma, T., and Wilhelm, D. (2025). A Powerful Bootstrap Test of Independence in High Dimensions. Preprint, arXiv:2503.21715.
#' @keywords block mutiplier bootstrap Chatterjee Rank Correlation
#' @importFrom compiler cmpfun
#' @examples
#' \dontrun{
#' n <- 100
#' p <- 10
#' dat <- list()
#' dat$X <- rnorm(n)
#' dat$Y <- MASS::mvrnorm(n = n, mu = rep(0,  p), Sigma=diag(rep(1, p)))
#' BMB.cv(dat, q=2, B=100, alpha=0.05, seed = 5) # default type is "bmb1"
#'
#' }
#' @export
BMB.cv <- function(dat, q, B, alpha, type = "bmb1", seed = NULL) {

    # Match the type while allowing for case-insensitivity matching
  type <- match.arg(tolower(type), choices = c("bmb", "bmb1", "bmb2"))
  # Check if the type is vald
  if (!type %in% c("bmb", "bmb1", "bmb2")) {
    stop("Invalid type. Choose one of 'bmb', 'bmb1', or 'bmb2'.")
  }
  if (!is.null(seed)) set.seed(seed) # set the seed if provided
  # Number of big blocks
  n <- length(dat$X)
  m <- floor((n - 1) / (q + 1))
  # Order the Y's according to X
  Yc <- dat$Y[order(dat$X), ]
  # CDF of concomitants for each test
  r <- apply(Yc, MARGIN = 2, rank) / n
  # W vectors
  W <- 2 - 3 * abs(r[-1, ] - r[-n, ]) - 6 * r[-n, ] * (1 - r[-n, ])
  # Big blocks sums
  A <- matrix(NA, nrow=m, ncol=dim(Yc)[2])
  for (j in 1:m) A[j,] <- colSums(W[((j-1)*(q+1)+1):((j-1)*(q+1)+q), ,drop=FALSE])

  if (type == "bmb") {
    # No studentization
    cv <- quantile(replicate(n=B, max(rnorm(m) %*% A / sqrt(m * q))), 1 - alpha)
  } else if (type == "bmb1") {
    # Studentization based on E[Aj^2]/q
    # Finite-sample variance of xi (approx. 0.4 for large n)
    vn <- n * (n - 2) * (4 * n - 7 ) / (10 * (n - 1)^2 * (n + 1))
    A1  <- sqrt(vn) * A / sqrt(0.4 + 1 / (10 * q))
    cv <- quantile(replicate(n=B, max(rnorm(m) %*% A1 / sqrt(m * q))), 1 - alpha)
  } else if (type == "bmb2") {
    # studentization based on the sample average of Aj^2/q
    # Finite-sample variance of xi (approx. 0.4 for large n)
    vn <- n * (n-2) * (4 * n - 7) / (10 * (n - 1)^2 * (n + 1))
    for (i in 1:dim(Yc)[2]) A[, i] <- A[, i] - mean(A[, i])
    for (i in 1:dim(Yc)[2]) A[, i] <- sqrt(vn) * A[, i]/sqrt(mean((A[,i]^2) / q))
    cv <- quantile(replicate(n = B, max(rnorm(m) %*% A / sqrt(m * q))), 1 - alpha)
  } else {
    stop("Invalid type")
  }

  # List with outputs
  output <- list()
  output$cv <- cv
  output$type <- type

  return(output)
}
BMB.cv <- compiler::cmpfun(BMB.cv)