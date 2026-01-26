library(parallel)
library(doParallel)
library(foreach)
library(DEoptim)
library(AER)
library(censReg)

### Load the data
Data <- read.csv("C:/Users/Math 564 Cabin 1/Documents/NLSY97.csv")
head(Data)
n <- nrow(Data)

# Check censoring
index <- which(Data$hours == 0) 
left_censored_count <- length(index)
censoring_pct <- (left_censored_count / n) * 100
cat("Censoring percentage:", censoring_pct, "%\n")
cat("Left-censored observations:", left_censored_count, "out of", n, "\n")

## First stage regression (for endogeneity correction)
first_stage <- lm(nwifeinc ~ speduc + age + educ + exper + expersq + 
                    kidslt6 + kidsge6 + black + hispanic, data = Data)
Data$res <- residuals(first_stage)


#### TOBIT MLE (Control Function Approach) ####
tobit_mle <- censReg(hours ~ age + nwifeinc + educ + exper + expersq + 
                       kidslt6 + kidsge6 + black + hispanic + res,
                     left = 0, data = Data)

# Extract coefficients (excluding log(sigma))
beta_mle <- coef(tobit_mle)
# Remove the sigma parameter if present
if("logSigma" %in% names(beta_mle)) {
  beta_mle <- beta_mle[!names(beta_mle) %in% "logSigma"]
}

set.seed(123)
initial_value <- beta_mle+rnorm(length(beta_mle),0,0.05)

### 1. ABSOLUTE LOSS FUNCTION (LAD TOBIT ESTIMATOR)
ABL_4 <- function(beta, x) {
  g_f <- beta[1] + beta[2]*x$age + beta[3]*x$nwifeinc + beta[4]*x$educ + 
    beta[5]*x$exper + beta[6]*x$expersq + beta[7]*x$kidslt6 + 
    beta[8]*x$kidsge6 + beta[9]*x$black + beta[10]*x$hispanic + beta[11]*x$res
  pred <- pmax(0, g_f)
  L <- sum(abs(x$hours - pred))
  return(L)
}

# Use differential evolution for global optimization
#de_res <- DEoptim(
  #fn = function(b) ABL_4(b, Data),
  #lower=initial_value-50,
  #upper=initial_value+50,
 # control = DEoptim.control(itermax = 1000, trace = 100)
#)
#est_abl_4 <- de_res$optim$bestmem

est_abl_4<-optim(initial_value, function(u) ABL_4 (u, Data))$par
names(est_abl_4) <- c("Intercept", "age", "nwifeinc", "educ", "exper", 
                      "expersq", "kidslt6", "kidsge6", "black", "hispanic", "res")

cat("\n=== LAD TOBIT ESTIMATES ===\n")
print(est_abl_4)

#### COMPUTE ASYMPTOTIC STANDARD ERRORS ####

# Design matrices
X_M <- as.matrix(cbind(1, Data[, c("age", "nwifeinc", "educ", "exper", 
                                   "expersq", "kidslt6", "kidsge6", 
                                   "black", "hispanic", "res")]))
colnames(X_M)[1] <- "Intercept"

Z_M <- as.matrix(cbind(1, Data[, c("age", "speduc", "educ", "exper", 
                                   "expersq", "kidslt6", "kidsge6", 
                                   "black", "hispanic")]))
colnames(Z_M)[1] <- "Intercept"

### define the response variable
Y <- Data$hours

### define the first-stage residuals
e_1 <- Data$res

# Second stage residuals
hat_eps<-Y - pmax(0, X_M %*% est_abl_4)

# Bandwidth for density estimation
h <- density(hat_eps)$bw

cat("\n=== COMPUTING COVARIANCE MATRIX ===\n")

## Estimate Sigma_2_Beta
Sigma_2B <- matrix(0, nrow = ncol(X_M), ncol = ncol(X_M))

for (i in 1:n) {
  indicator1 <- as.numeric(sum(X_M[i, ] * est_abl_4) > 0)
  indicator2 <- as.numeric(abs(hat_eps[i]) <= h)
  Sigma_2B <- Sigma_2B + indicator1 * indicator2 * outer(X_M[i, ], X_M[i, ])
}

hat_Sigma_2B <- Sigma_2B / (2 * n * h)

## Estimate Sigma_2_Delta
Sigma_2D <- matrix(0, nrow = ncol(X_M), ncol = ncol(Z_M))

for (i in 1:n) {
  indicator1 <- as.numeric(sum(X_M[i, ] * est_abl_4) > 0)
  indicator2 <- as.numeric(abs(hat_eps[i]) <= h)
  Sigma_2D <- Sigma_2D + indicator1 * indicator2 * outer(X_M[i, ], Z_M[i, ])
}

hat_rho_10 <- est_abl_4["res"]
hat_Sigma_2D <- hat_rho_10 * Sigma_2D / (2 * n * h)

## Estimate Sigma_1_Delta
Sigma_1D <- matrix(0, ncol(Z_M), ncol(Z_M))

for (i in 1:n) {
  z_i <- matrix(Z_M[i, ], ncol = 1)
  Sigma_1D <- Sigma_1D + z_i %*% t(z_i)
}
hat_Sigma_1D <- Sigma_1D / n

## Estimate D_1
S_eZZ <- matrix(0, ncol(Z_M), ncol(Z_M))

for (i in 1:n) {
  z_i <- matrix(Z_M[i, ], ncol = 1)
  S_eZZ <- S_eZZ + (e_1[i]^2) * (z_i %*% t(z_i))
}
hat_D_1 <- S_eZZ / n

## Estimate D_2
D_2 <- matrix(0, nrow = ncol(X_M), ncol = ncol(X_M))

for (i in 1:n) {
  indicator1 <- as.numeric(sum(X_M[i, ] * est_abl_4) > 0)
  D_2 <- D_2 + indicator1 * outer(X_M[i, ], X_M[i, ])
}
hat_D_2 <- D_2 / (4 * n)

## Compute Omega_1
inv_hat_Sigma_1D <- solve(hat_Sigma_1D)
hat_Omega_1 <- inv_hat_Sigma_1D %*% hat_D_1 %*% inv_hat_Sigma_1D

## Compute final covariance matrix
inv_hat_Sigma_2B <- solve(hat_Sigma_2B)
Middle <- hat_D_2 + hat_Sigma_2D %*% hat_Omega_1 %*% t(hat_Sigma_2D)
hat_S <-(1/n) * inv_hat_Sigma_2B %*% Middle %*% inv_hat_Sigma_2B

## Standard errors
SE <-sqrt(diag(hat_S))
names(SE) <- names(est_abl_4)

cat("\n=== STANDARD ERRORS ===\n")
print(SE)

## 95% Confidence Interval for coefficient on 'res' (test for endogeneity)
p <- ncol(X_M)
t_crit <- qt(0.975, n - p)

SE_res <- SE["res"]
L_B <- est_abl_4["res"] - t_crit * SE_res
U_B <- est_abl_4["res"] + t_crit * SE_res
CI_res <- c(L_B, U_B)

cat("\n=== 95% CONFIDENCE INTERVAL FOR 'res' COEFFICIENT ===\n")
cat("Coefficient:", est_abl_4["res"], "\n")
cat("Standard Error:", SE_res, "\n")
cat("95% CI: [", L_B, ",", U_B , "]\n")

if (L_B  > 0 | U_B  < 0) {
  cat("\n*** 'res' is SIGNIFICANT - nwifeinc is ENDOGENOUS ***\n")
} else {
  cat("\n*** 'res' is NOT significant - nwifeinc may be EXOGENOUS ***\n")
}

## Summary table
cat("\n=== FINAL RESULTS TABLE ===\n")
results_table <- data.frame(
  Coefficient = est_abl_4,
  Std_Error = SE,
  t_value = est_abl_4 / SE,
  CI_Lower = est_abl_4 - t_crit * SE,
  CI_Upper = est_abl_4 + t_crit * SE
)
##Add length of the confidence interval
results_table$CI_Length <- results_table$CI_Upper - results_table$CI_Lower
print(round(results_table, 4))


## Save results
results <- list(
  coefficients = est_abl_4,
  std_errors = SE,
  covariance_matrix = hat_Sigma,
  CI_res = CI_res,
  censoring_pct = censoring_pct,
  n = n,
  first_stage = summary(first_stage)
)

cat("\n=== ANALYSIS COMPLETE ===\n")