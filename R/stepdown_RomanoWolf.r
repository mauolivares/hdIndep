#' @title Romano-Wolf Stepdown Method
#'
#' @description Consider a family of individual null hypotheses \eqn{H_{0,j}: Y_j \perp X}, for \eqn{j=1,\dots,p}. This function implements a stepwise procedure for selecting those variables from \eqn{Y_1,\dots,Y_p} that violate independence, while controlling the family-wise error rate.
#' @param dat Data frame. There are two elements in the data frame, X and D, where X is a random variable and D corresponds with the \eqn{Y_j} in the individual hypothesis \eqn{Y_j \perp X}.
#' @param q Numeric. The block size for the block multiplier bootstrap.
#' @param B Numeric. The number of bootstrap replications.
#' @param alpha Numeric. The significance level.
#' @param type Character. The type of the block multiplier bootstrap.
#' @param seed Numeric. The seed for the random number generator. If \code{NULL}, the seed is not set. If a positive integer, it sets the seed for reproducibility.
#' @param steps Logical. If \code{TRUE}, the function will return the steps of the stepdown procedure. The default is \code{FALSE}.
#' @return Returns a list with the total number of rejections. If steos = \code{TRUE}, it will also return the steps of the stepdown procedure.
#' @author Maurcio Olivares
#' @author Tomasz Olma
#' @author Daniel Wilhelm
#' @references
#' Olivares, M., Olma, T., and Wilhelm, D. (2025). A Powerful Bootstrap Test of Independence in High Dimensions. Preprint, arXiv:2503.21715.
#' @keywords Romano-Wolf Stepdown Block Multiplier Bootstrap Chatterjee Rank Correlation
#' @include BMB.cv.r
#' @import compiler
#' @export



# Stepdown Romano-Wolf procedure to get critical values
stepdown_RomanoWolf <- function(dat, q, B, alpha, type=c("bmb", "bmb1", "bmb2"), seed = NULL, steps = FALSE) {
  # Check if seed is NULL or a positive integer
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || seed <= 0 || seed != round(seed))) {
    stop("Seed must be a positive integer or NULL.")
  }
  type <- match.arg(type)
  # Check if type is a valid one
  if (!type %in% c("bmb", "bmb1", "bmb2")) {
    stop("Invalid type. Choose one of 'bmb', 'bmb1', or 'bmb2'.")
  }
  # Check if dat is a data frame with the required elements
  if (!is.data.frame(dat) || !all(c("X", "D") %in% names(dat))) {
    stop("dat must be a data frame with elements X and D.")
  }
  D <- dat$D
  p <- ncol(D)
  n <- nrow(D)
  X <- dat$X
  stopifnot(nrow(X) == nrow(D))

  # List with the rejected hypotheses
  output <- list()
  output$rejected_total <- numeric()

  # If steps = TRUE, we will store the steps of the stepdown procedure
  if (steps) {
    output$counter <- 1
    output$rejected <- list()
  }

  # Initialize
  tstats <- sqrt(n) * sapply(1:p, function(j) xi_chatterjee(dat$X, D[, j]))
  active <- 1:p

  repeat {
    cv <- BMB.cv(list(X = X, Y = D[, active]), q, B, alpha, type, seed)$cv
    if (all(tstats[active] <= cv)) break
    active <- intersect(which(tstats <= cv), active)
    if (steps) {
      output$rejected[[output$counter]] <- setdiff(1:p, active)
      output$counter <- output$counter + 1
    }
    if (length(active) == 0) break
  }
  # Store the total rejections
  output$rejected_total <- setdiff(1:p, active)
  # Return the output
  return(output)
}

stepdown_RomanoWolf <- cmpfun(stepdown_RomanoWolf)


"xi_chatterjee" <- function(xvec, yvec, simple = TRUE) {
  # It is verbatim what the XICOR package provides but
  # coded here so we can run it on the HPC.
  n <- length(xvec)
  PI <- rank(xvec, ties.method = "random")
  fr <- rank(yvec, ties.method = "max")/n
  gr <- rank((-yvec), ties.method = "max")/n
  ord <- order(PI)
  fr <- fr[ord]
  A1 <- sum(abs(fr[1:(n - 1)] - fr[2:n]))/(2 * n)
  CU <- mean(gr * (1 - gr))
  xi <- 1 - A1 / CU
  if (simple == TRUE) return(xi)
  else return(list(xi = xi, fr = fr, CU = CU))
}