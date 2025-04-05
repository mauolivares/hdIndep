#' @title Optimal Block Size for Block Multiplier Bootstrap.
#'
#' @description Calculates size of the blocks needed for the block multiplier bootstrap, denoted as \eqn{q}, using the rule of thumb described in Olivares, Olma, and Wilhelm (2025). 
#' @param n Numeric. Sample size
#' @return Returns an integer value corresponding to the optimal block size.
#' @author Maurcio Olivares
#' @author Tomasz Olma
#' @author Daniel Wilhelm
#' @references
#' Olivares, M., Olma, T., and Wilhelm, D. (2025). A Powerful Bootstrap Test of Independence in High Dimensions. Preprint, arXiv:2503.21715.
#' @keywords Block Multiplier Bootstrap
#' @importFrom compiler cmpfun
#' @export
block_size <- function(n) {
# Check if n is a positive integer
if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != round(n)) {
  stop("Sample size must be a positive integer")
}
  # Two candidate values
  q <- max(1, (n / 16)^(1 / 3))
  q1 <- floor(q)
  q2 <- ceiling(q)

  qstar <- ifelse(mse_q(q1, n) < mse_q(q2, n), q1, q2)
  # Return the optimal block size
  qstar
}
block_size <- compiler::cmpfun(block_size)

"mse_q" <- function(q, n) {
  # Check if q is a positive integer
  if (!is.numeric(q) || length(q) != 1 || q <= 0 || q != round(q)) {
    stop("Block size must be a positive integer")
  }
  if (q == 1) {
    a <- 7 / 20
  } else if (q == 2) {
    a <- 1353 / 2800
  } else if (q > 2) {
    a <- 8 / 25 + 88 / (175 * q) - 229 / (700 * q^2)
  }

  m <- floor((n - 1) / (q + 1))
  a / m + (n * (n - 2) * (4 * n - 7) / (10 * (n - 1)^2 * (n + 1)) - 0.4 - 0.1 / q)^2
}