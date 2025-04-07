#' Summarizing Block Multiplier Bootstrap Test of Independence in High Dimensions.
#' 
#' \code{summary} method for class \code{"BMB_indep"}
#' 
#' @method summary BMB_indep
#' @param object an object of class \code{"BMB_indep"}, usually a result of a call to \code{\link{BMB_indep}}.
#' @param digits number of digits to display
#' @param ... unused
#' @return \code{summary_BMB_indep} returns an object of \link{class} "\code{summary.BMB_indep}" which has the following components
#'  \item{results}{Matrix with the Testing Problem, Type of Test Statistics, Test Statistics, Sample Size, Number of Hypothesis, Significance Level, Block size, Number of bootrap samples, and Block-Multiplier-Bootstrap critical value.}
#' @author Maurcio Olivares
#' @author Tomasz Olma
#' @author Daniel Wilhelm
#' @export

summary.BMB_indep <- function(object, ..., digits = 4) {
  cat("\n")
  cat("**************************************************\n")
  cat("**   Powerful Block-Multiplier Bootstrap Test   **\n")
  cat("**   of Independence in High Dimensions         **\n")
  cat("**************************************************\n")
  cat("* -----------------------------------------------*\n")
  cat("     H0: X independent of Y1, Y2, ..., Yp\n")
  cat("* -----------------------------------------------*\n\n")

  cat("Number of Hypotheses:       ", object$p, "\n")
  cat("Sample Size:                ", object$n, "\n")
  cat("Value of Test Statistic:    ", formatC(object$T_obs, digits = digits, format = "f"), "\n")
  cat("Bootstrap Critical Value:   ", formatC(object$cv, digits = digits, format = "f"), "\n")

  if (object$type == "bmb") {
    cat("Critical value computed from the conditional distribution of the unstudentized bootstrap test statistic.\n")
  } else if (object$type == "bmb1") {
    cat("Critical value computed from the conditional distribution of the bootstrap statistic\n")
    cat("studentized by the square root of the big block's second moment divided by the block size.\n")
  } else if (object$type == "bmb2") {
    cat("Critical value computed from the conditional distribution of the bootstrap statistic\n")
    cat("studentized by standardized demeaned big blocks.\n")
  }

  cat("Optimal Block Size:         ", object$block_size, "\n")
  cat("Number of Bootstrap Samples:", object$B, "\n")
  cat("Decision:                   ", object$decision, " at significance level ", object$alpha, "\n")

  invisible(object)
}
