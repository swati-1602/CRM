library(parallel)
library(doParallel)
library(foreach)
library(DEoptim)
library(AER)
library(nloptr)
library(wooldridge)

### Load the data
Data <- read.csv("C:/Users/Math 564 Cabin 1/Documents/NLSY97.csv")

# Look at the first few rows
head(Data)


# Number of observations
n <- nrow(Data)


# Indices where hours == 0
index <- which(Data$hours == 0)


# Plot hours against observation index
plot(1:n, c(Data$hours[Data$hours == 0], Data$hours[Data$hours!=0]),
     xlab = "Index",
     ylab = "Hours Worked",
     main = "Hours vs Index")
# Count left-censored observations
left_censored_count <- length(index)
censoring <- (left_censored_count/1147)*100

cat("Censoring percentage:", censoring, "%\n")

## first stage regression 
Data$res <- residuals(lm(nwifeinc ~ speduc + age + educ + exper + expersq + 
                           kidslt6 + kidsge6 + black + hispanic, data = Data))


#### TOBIT MLE WITH CONTROL FUNCTION TO FIND INITAL VALUE####
tobit_mle <- tobit(hours ~ age + nwifeinc + educ + exper + expersq + 
                     kidslt6 + kidsge6 + black + hispanic + res,
                   data = Data)

cat("\n=== TOBIT MLE (WITH CONTROL FUNCTION) ===\n")
summary(tobit_mle)

## ===== Extract initial values safely =====
beta_mle <- coef(tobit_mle)
beta_mle <- beta_mle[!names(beta_mle) %in% "logSigma"]
set.seed(123)
initial_value <- beta_mle+rnorm(length(beta_mle),0,0.05)

cat("\n=== INITIAL VALUES FROM TOBIT MLE ===\n")
print(initial_value)


### second-step
## ===== Define Huber loss (CORRECTED VARIABLE ORDER) =====
HBL_4 <- function(beta, x) {
  ## CORRECT ORDER: age, nwifeinc, educ, exper, expersq, kidslt6, kidsge6, black, hispanic, res
  g_f <- beta[1] +
    beta[2]  * x$age +
    beta[3]  * x$nwifeinc +
    beta[4]  * x$educ +
    beta[5]  * x$exper +
    beta[6]  * x$expersq +
    beta[7]  * x$kidslt6 +
    beta[8]  * x$kidsge6 +
    beta[9]  * x$black +
    beta[10] * x$hispanic +
    beta[11] * x$res
  
  # Tobit prediction (left-censoring at 0)
  pred <- pmax(0, g_f)
  
  # Residuals
  r <- x$hours - pred
  r_abs <- abs(r)
  delta<-6.945981*mad(r)
  #delta<-sd(r)
  # Huber loss
  loss <- ifelse(r_abs <= delta,
                 0.5 * r^2,
                 delta * (r_abs - 0.5 * delta))
  
  return(sum(loss))
}

cat("\n=== OPTIMIZING WITH DEoptim ===\n")
#de_res <- DEoptim(fn = function(b) HBL_4(b, Data),
#lower = initial_value - 50, 
#upper = initial_value + 50,
#control = DEoptim.control(itermax = 1000, trace = 100))

#est_Hbl_4 <- de_res$optim$bestmem

est_Hbl_4<-optim(initial_value, function(u) HBL_4 (u, Data))$par

# CRITICAL: NAME THE COEFFICIENTS
names(est_Hbl_4) <- c("Intercept", "age", "nwifeinc", "educ", "exper", 
                      "expersq", "kidslt6", "kidsge6", "black", "hispanic", "res")

p <- length(est_Hbl_4)

cat("\n=== LAD TOBIT ESTIMATES ===\n")
print(est_Hbl_4)

## Define terms to compute Confidence Interval

# CORRECTED: Use explicit variable names instead of column positions
X_M <- as.matrix(cbind(1, Data[, c("age", "nwifeinc", "educ", "exper", 
                                   "expersq", "kidslt6", "kidsge6", 
                                   "black", "hispanic", "res")]))
colnames(X_M)[1] <- "Intercept"

# CORRECTED: Proper instrument matrix (speduc replaces nwifeinc, no res)
Z_M <- as.matrix(cbind(1, Data[, c("age", "speduc", "educ", "exper", 
                                   "expersq", "kidslt6", "kidsge6", 
                                   "black", "hispanic")]))
colnames(Z_M)[1] <- "Intercept"

cat("\n=== DESIGN MATRICES ===\n")
cat("X_M dimensions:", dim(X_M), "\n")
cat("X_M columns:", colnames(X_M), "\n")
cat("Z_M dimensions:", dim(Z_M), "\n")
cat("Z_M columns:", colnames(Z_M), "\n")

## response variable
Y <- Data$hours

## First Stage residual
e_1 <- Data$res

## Second Stage Residual
#hat_eps <- sapply(1:n, function(i) (Y[i] - sum(X_M[i, ] * est_Hbl_4)))
hat_eps<-Y - pmax(0,X_M %*% est_Hbl_4)

# This is robust to outliers and commonly used
#delta_mad <- 1.345 * mad(hat_eps)  # 1.345*MAD ≈ SD for normal distribution
#delta <- delta_mad

# First derivative (psi function) for huber loss
huber_psi <- function(r) {
  delta<-6.945981*mad(r)
  #delta<-4.685*1.4826*mad(r)
  r_abs <- abs(r)
  psi <- r
  psi[r_abs > delta] <- delta * sign(r[r_abs > delta])
  return(psi)
}

# Second derivative (psi prime)fOR huber loss
huber_psi_prime <- function(r) {
  delta<-6.945981*mad(r)
  #delta<-sd(r)
  psi_p <- rep(1, length(r))
  psi_p[abs(r) > delta] <- 0
  return(psi_p)
}
# Psi evaluated at residuals
psi_hat<- huber_psi(hat_eps)

# Psi' evaluated at residuals
psi_p_hat<- huber_psi_prime(hat_eps)

cat("\n=== COMPUTING ASYMPTOTIC COVARIANCE MATRIX ===\n")

## Estimate of Sigma_2_Beta

# Indicator vector (compute once, not inside loop)
I_pos<- as.numeric(X_M %*% est_Hbl_4 > 0)

# Initialize matrix
S2BH<- matrix(0, nrow = ncol(X_M), ncol = ncol(X_M))

# Compute the sum
for (i in 1:n) {
  xi <- matrix(X_M[i, ], ncol = 1)
  S2BH<- S2BH + I_pos[i] * psi_p_hat[i] * (xi %*% t(xi))
}
# Divide by n
S2BH_hat <- S2BH / n

S2BH_hat

## Estimate of Sigma_2_Delta

# Initialize p x q matrix
S2DH<- matrix(0, nrow = ncol(X_M), ncol = ncol(Z_M))

for (i in 1:n) {
  xi <- matrix(X_M[i, ], nrow = 1)   # 1 x p
  zi <- matrix(Z_M[i, ], ncol = 1)   # q x 1
  
  S2DH <- S2DH +
    I_pos[i] * psi_p_hat[i] * (t(xi) %*% t(zi))
}

# CORRECTED: Use named coefficient
hat_rho_10_h <- est_Hbl_4["res"]
cat("Coefficient on 'res' (hat_rho_10):", hat_rho_10_h, "\n")

# Average
S2DH_hat <- hat_rho_10_h * S2DH / n
S2DH_hat


## Estimate of Sigma_1_Delta 
S1DH<- matrix(0, ncol(Z_M), ncol(Z_M))

for (i in 1:n) {
  z_i <- matrix(Z_M[i, ], ncol = 1)   # column vector
  S1DH <- S1DH+ z_i %*% t(z_i)
}
hat_S1DH <- S1DH / n
hat_S1DH

### estimate of D_1  
D1H <- matrix(0, ncol(Z_M), ncol(Z_M))

for (i in 1:n) {
  z_i <- matrix(Z_M[i, ], ncol = 1)
  D1H <- D1H + (e_1[i]^2) * (z_i %*% t(z_i))
}
hat_D1H <- D1H / n
hat_D1H

### estimate of D_2 
# Initialize matrix
D2H <- matrix(0, nrow = ncol(X_M), ncol = ncol(X_M))

# Compute the sum
for (i in 1:n) {
  xi <- matrix(X_M[i, ], ncol = 1)
  D2H <- D2H + I_pos[i] * (psi_hat[i])^2 * (xi %*% t(xi))
}
# Divide by n
D2H_hat <- D2H / n
D2H_hat

### define hat_omega_1
inv_hat_S1DH <- solve(hat_S1DH)
hat_Omega_1H <- inv_hat_S1DH %*% hat_D1H %*% t(inv_hat_S1DH)

### find covariance matrix sigma

### first define inverse of hat_Sigma_2Beta
inv_S2BH_hat <- solve(S2BH_hat)

# Middle term: hat_D_2 + hat_Sigma_2Delta * hat_Omega_1 * hat_Sigma_2Delta'
Middle <- D2H_hat + S2DH_hat %*% hat_Omega_1H %*% t(S2DH_hat)

# Final COVARIANCE MATRIX HAT_SIGMA
hat_Sigma_H <- (1/n) * inv_S2BH_hat %*% Middle %*% t(inv_S2BH_hat)

### 95% confidence interval of beta
# Standard errors (sqrt of diagonal)
SE_beta_H <- sqrt(diag(hat_Sigma_H))
names(SE_beta_H) <- names(est_Hbl_4)

cat("\n=== STANDARD ERRORS ===\n")
print(SE_beta_H)

# Critical value
crit_t <- qt(0.975, n - p)

# Confidence intervals
CI_l_H <- est_Hbl_4 - crit_t * SE_beta_H
CI_u_H <- est_Hbl_4 + crit_t * SE_beta_H

# Combine into a table
CI_table <- data.frame(
  Estimate = est_Hbl_4,
  Std.Error = SE_beta_H,
  t_value = est_Hbl_4 / SE_beta_H,
  Lower_95 = CI_l_H,
  Upper_95 = CI_u_H
)
# Add length of the confidence interval
CI_table$CI_Length <- CI_table$Upper_95 - CI_table$Lower_95
cat("\n=== FINAL RESULTS TABLE ===\n")
print(round(CI_table, 4))

# Test for endogeneity (test if 'res' coefficient is significant)
cat("\n=== TEST FOR ENDOGENEITY ===\n")
cat("Coefficient on 'res':", est_Hbl_4["res"], "\n")
cat("Standard Error:", SE_beta_H["res"], "\n")
cat("t-statistic:", est_Hbl_4["res"] / SE_beta_H["res"], "\n")
cat("95% CI: [", CI_l_H["res"], ",", CI_u_H["res"], "]\n")

if (CI_l_H["res"] > 0 | CI_u_H["res"] < 0) {
  cat("\n*** 'res' is SIGNIFICANT - nwifeinc is ENDOGENOUS ***\n")
} else {
  cat("\n*** 'res' is NOT significant - nwifeinc may be EXOGENOUS ***\n")
}

# Save results
results <- list(
  coefficients = est_Hbl_4,
  std_errors = SE_beta_H,
  covariance_matrix = hat_Sigma_H,
  CI_table = CI_table,
  censoring_pct = censoring,
  n = n
)

save(results, file = "corrected_results.RData")
cat("\n=== RESULTS SAVED ===\n")

