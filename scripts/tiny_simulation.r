# This is a script with a single simulation step, mainly to check whether
# the package was installed successfully.
# It's supposed to be as self-contained as possible.

library(hdIndep)
library(MASS)

# Parameters
n <- 100
p <- 100
q <- block_size(n) # Block size
alpha <- 0.05
B <- 1000
type <- "bmb1"
seed <- 5
# Parameters of the DGP
tau <- 0.5
rho <- 0.5


# Function to generate data
gen_data <- function(n, p, tau = 0, rho = 0) {

  if (p < 2) {
    stop("The number of individual hypotheses must be greater than 2")
  }
  # Generate the data
  Sigma <- matrix(0, nrow = (p + 1), ncol = (p + 1))
  diag(Sigma) <- 1
  Sigma[1, 2] <- rho
  Sigma[2, 1] <- rho
  Sigma[3, 2] <- tau
  Sigma[2, 3] <- tau

  D <- MASS::mvrnorm(n = n, mu = rep(0, p + 1), Sigma = Sigma)
  Y <- D[, 2:(p + 1)]
  X <- D[, 1]

  return(list(X = X, Y = Y))
}


# Generate data
dat <- gen_data(n = n, p = p, rho = rho, tau = tau)
  
# Calculate the test statistic as the maximum of many Chatterjee's rank correlations
max_stats <- max.stat(dat)
stepdown <- stepdown_RomanoWolf(dat, q, B, alpha, type, seed, steps = TRUE)
# Perform the BMB test
test_bmb <- BMB.cv(dat, q, B, alpha, type, seed)
cat("Printing the result.\n")
cat("Test statistic:", max_stats, "\n")
cat("Block multiplier bootstrap critical value:", test_bmb$cv, "\n")
cat("Type of test statistic:", test_bmb$type, "\n")
cat("Optimal block size using rule of thumb:", q, "\n")
cat("Number of rejected hypotheses:", length(stepdown$rejected_total), "\n")
cat("Proportion of rejected hypotheses:", length(stepdown$rejected_total) / p, "\n")
cat("Number of steps in the stepdown procedure:", stepdown$counter, "\n")
cat("Rejected hypotheses in each step:\n")
for (i in 1:stepdown$counter) {
  cat("Step", i, ":", stepdown$rejected[[i]], "\n")
}
