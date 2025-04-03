#' @title Blocks size for Block Multiplier Bootstrap.
#'
#' @description Calculates size of the blocks needed for the block multiplier bootstrap, denoted as \eqn{q}, using the rule of thumb described in Olivares, Olma, and Wilhelm (2025). The function returns \eqn{q} using the approximatation if the sample size is larger than 224. 
#' @param n Numeric. Sample size
#' @return Returns an integer value of block size.
#' @author Maurcio Olivares
#' @author Tomasz Olma
#' @author Daniel Wilhelm
#' @references
#' Olivares, M., Olma, T., and Wilhelm, D. (2025). A Powerful Bootstrap Test of Independence in High Dimensions. Preprint, arXiv:2503.21715.
#' @keywords Chatterjee Rank Correlation Bootstrap
#' @importFrom compiler cmpfun
#' @export
block_size <- function(n) {
# Check if n is a positive integer
if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != round(n)) {
  stop("Sample size must be a positive integer")
}
# Check if n is greater than 2 but smaller than or equal to 87
  if (n > 2 && n <= 87) {
  q <- 1
  } else if (n > 87 && n <= 224) {
    q <- 2
  } else {
    q <- (n / 16)^(1 / 3)
    # Round q to the nearest integer
    q <- round(q)
  }
  # Return the block size
  q
}

block_size <- compiler::cmpfun(block_size)
