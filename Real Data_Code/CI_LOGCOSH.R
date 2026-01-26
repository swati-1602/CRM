library(parallel)
library(doParallel)
library(foreach)
library(AER)
library(wooldridge)

### Load the data
Data <- read.csv("C:/Users/Math 564 Cabin 1/Documents/NLSY97.csv")
n <- nrow(Data)
index <- which(Data$hours == 0) 

# Count left-censored observations
left_censored_count <- length(index)
censoring <- (left_censored_count/1147)*100

cat("Censoring percentage:", censoring, "%\n")

## first stage regression 
Data$res <- residuals(lm(nwifeinc ~ speduc + age + educ + exper + expersq + 
                           kidslt6 + kidsge6 + black + hispanic, data = Data))



#### TOBIT MLE WITH CONTROL FUNCTION to find inital value ####
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



## ===============================
## Log-Cosh Tobit Loss Function
## ===============================

CLH_4 <- function(beta, x) {
  
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
  
  # Numerically stable log-cosh loss
  loss <- abs(r) + log1p(exp(-2 * abs(r))) - log(2)
  
  L <- sum(loss)
  
  # Safety check
  if (!is.finite(L)) return(1e10)
  
  return(L)
}


cat("\n=== OPTIMIZING WITH DEoptim ===\n")
#de_res <- DEoptim(fn = function(b) CLH_4(b, Data),
#lower = initial_value - 50, 
#upper = initial_value + 50,
#control = DEoptim.control(itermax = 1000, trace = 100))
#est_CLH_4<- de_res$optim$bestmem

est_CLH_4<-optim(initial_value, function(u) CLH_4 (u, Data))$par

# CRITICAL: NAME THE COEFFICIENTS
names(est_CLH_4) <- c("Intercept", "age", "nwifeinc", "educ", "exper", 
                      "expersq", "kidslt6", "kidsge6", "black", "hispanic", "res")

p <- length(est_CLH_4)

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
Y <-Data$hours

## First Stage residual
e_1 <-Data$res

## Second Stage Residual
#hat_eps <- sapply(1:n, function(i) (Y[i] - sum(X_M[i, ] * est_CLH_4)))
hat_eps<-Y - pmax(0, X_M %*% est_CLH_4)

# First derivative for Logcosh()
d_logcosh <- function(l) {
  tanh(l)
}

# Second derivative for Logcosh()
dd_logcosh <- function(l) {
  pmax(1 - tanh(l)^2, 1e-03)
}

# First derivative for Logcosh() evaluated at residuals
psi_c<- d_logcosh(hat_eps)
psi_p_c<-dd_logcosh(hat_eps)
cat("\n=== COMPUTING ASYMPTOTIC COVARIANCE MATRIX ===\n")

## Estimate of Sigma_2_Beta

# Indicator vector (compute once, not inside loop)
I_pos<- as.numeric(X_M %*% est_CLH_4 > 0)

# Initialize matrix
S2BC<- matrix(0, nrow = ncol(X_M), ncol = ncol(X_M))

# Compute the sum
for (i in 1:n) {
  xi <- matrix(X_M[i, ], ncol = 1)
  S2BC<- S2BC + I_pos[i] * psi_p_c[i] * (xi %*% t(xi)) 
}
# Divide by n
S2BC_hat <- S2BC / n


## Estimate of Sigma_2_Delta

S2DC <- matrix(0, nrow = ncol(X_M), ncol = ncol(Z_M))

for (i in 1:n) {
  xi <- matrix(X_M[i, ], ncol = 1)   # p x 1
  zi <- matrix(Z_M[i, ], ncol = 1)   # q x 1
  
  S2DC <- S2DC +
    I_pos[i] * psi_p_c[i] * (xi %*% t(zi))
}

# CORRECTED: Use named coefficient
hat_rho_10_c <- est_CLH_4["res"]

# Average
S2DC_hat <- hat_rho_10_c * S2DC / n

## Estimate of Sigma_1_Delta 
S1DC<- matrix(0, ncol(Z_M), ncol(Z_M))

for (i in 1:n) {
  z_i <- matrix(Z_M[i, ], ncol = 1)   # column vector
  S1DC <- S1DC+ z_i %*% t(z_i)
}
hat_S1DC <- S1DC / n
hat_S1DC

### estimate of D_1  
D1C <- matrix(0, ncol(Z_M), ncol(Z_M))

for (i in 1:n) {
  z_i <- matrix(Z_M[i, ], ncol = 1)
  D1C <- D1C + (e_1[i]^2) * (z_i %*% t(z_i))
}
hat_D1C <- D1C / n
hat_D1C

### estimate of D_2 
# Initialize matrix
D2C <- matrix(0, nrow = ncol(X_M), ncol = ncol(X_M))

# Compute the sum
for (i in 1:n) {
  xi <- matrix(X_M[i, ], ncol = 1)
  D2C <- D2C + I_pos[i] * (psi_c[i])^2 * (xi %*% t(xi))
}
# Divide by n
D2C_hat <- D2C / n
D2C_hat

### define hat_omega_1
inv_hat_S1DC <- solve(hat_S1DC)
hat_Omega_1C <- inv_hat_S1DC %*% hat_D1C %*% t(inv_hat_S1DC)

### find covariance matrix sigma

### first define inverse of hat_Sigma_2Beta
inv_S2BC_hat <- solve(S2BC_hat)

# Middle term: hat_D_2 + hat_Sigma_2Delta * hat_Omega_1 * hat_Sigma_2Delta'
middle <- D2C_hat + S2DC_hat %*% hat_Omega_1C %*% t(S2DC_hat)

# Final COVARIANCE MATRIX HAT_SIGMA
hat_Sigma_C <- (1/n) * inv_S2BC_hat %*% middle %*% t(inv_S2BC_hat)

### 95% confidence interval of beta
# Standard errors (sqrt of diagonal)
SE_beta_C <- sqrt(diag(hat_Sigma_C))
names(SE_beta_C) <- names(est_CLH_4)

cat("\n=== STANDARD ERRORS ===\n")
print(SE_beta_C)

# Critical value
crit_t <- qt(0.975, n - p)

# Confidence intervals
CI_l_C <- est_CLH_4 - crit_t * SE_beta_C
CI_u_C <- est_CLH_4 + crit_t * SE_beta_C

# Combine into a table
CI_table <- data.frame(
  Estimate = est_CLH_4,
  Std.Error = SE_beta_C,
  t_value = est_CLH_4 / SE_beta_C,
  L_95 = CI_l_C,
  U_95 = CI_u_C
)
# Add length of the confidence interval
CI_table$CI_Length <- CI_table$U_95 - CI_table$L_95
cat("\n=== FINAL RESULTS TABLE ===\n")
print(round(CI_table, 4))

# Test for endogeneity (test if 'res' coefficient is significant)
cat("\n=== TEST FOR ENDOGENEITY ===\n")
cat("Coefficient on 'res':", est_CLH_4["res"], "\n")
cat("Standard Error:", SE_beta_C["res"], "\n")
cat("t-statistic:", est_CLH_4["res"] / SE_beta_C["res"], "\n")
cat("95% CI: [", CI_l_C["res"], ",", CI_u_C["res"], "]\n")

if (CI_l_C["res"] > 0 | CI_u_C["res"] < 0) {
  cat("\n*** 'res' is SIGNIFICANT - nwifeinc is ENDOGENOUS ***\n")
} else {
  cat("\n*** 'res' is NOT significant - nwifeinc may be EXOGENOUS ***\n")
}

# Save results
results <- list(
  coefficients = est_CLH_4,
  std_errors = SE_beta_C,
  covariance_matrix = hat_Sigma_C,
  CI_table = CI_table,
  censoring_pct = censoring,
  n = n
)

save(results, file = "corrected_results.RData")
cat("\n=== RESULTS SAVED ===\n")
