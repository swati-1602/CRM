library(ExtDist)
library(ggplot2)
library(cowplot)
library(MASS)
library(AER)
library(parallel)
library(foreach)
library(doParallel)
library(quantreg)
library(survival)

# Setup parallel processing
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Sample sizes
sample_size <- seq(50, 1000, 50)

## ---------------------------------------------------------------------
## 5. RHO (LOSS) FUNCTIONS
## ________________________________________________________________________

# Huber Loss
huber_loss <- function(r, c) {
  loss <- ifelse(abs(r) <= c, 0.5 * r^2, c * (abs(r) - 0.5 * c))
  return(loss)
}

# Hampel Loss
hampel_loss <- function(r, a, b, c) {
  
  x <- abs(r)
  rho <- numeric(length(x))
  
  # Region 1: 0 <= |r| < a
  rho[x < a] <- 0.5 * x[x < a]^2
  
  # Region 2: a <= |r| < b
  idx <- (x >= a & x < b)
  rho[idx] <- a * (x[idx] - a / 2)
  
  # Region 3: b <= |r| < c
  idx <- (x >= b & x < c)
  rho[idx] <- (a * (x[idx] - c)^2) / (2 * (b - c))+
    a * (b + c - a) / 2
  
  # Region 4: |r| >= c
  idx <- (x >= c)
  rho[idx] <- a * (b + c - a) / 2
  
  return(rho)
}

# Tukey bisquare loss function 
tukey_loss <- function(r, c ) {
  u <- r / c
  out <- ifelse(abs(u) <= 1,
                (c^2 / 6) * (1 - (1 - u^2)^3),
                c^2 / 6)
  return(out)
}

## quantile loss
quantile_loss <- function(r, tau = 0.75) {
  out<-ifelse(r >= 0, tau * r, (tau - 1) * r)
  return(out)
}

# SCLS Loss
scls_loss <- function(beta, y, x){
  
  # Linear predictor
  eta <- as.vector(x %*% beta)
  
  # First part
  part1 <- (y - pmax(0.5 * y, eta))^2
  
  # Second part
  part2 <- ifelse(
    y > 2 * eta,
    (0.5 * y)^2 - (pmax(0, eta))^2,
    0
  )
  
  sum(part1 - part2)
}

# Tunning constant
set.seed(123)
Tun_cons <- function(beta, n) {
  
  ## Instruments and regressors
  z   <- runif(n)
  e_1 <- rnorm(n)
  x_1 <- rnorm(n)
  
  ## Endogenous regressor
  x_2 <- beta[1] + beta[2]*x_1 + beta[3]*z + e_1
  
  # =========================
  # Stage 1: LSE (control function)
  # =========================
  res <- residuals(lm(x_2 ~ z + x_1))
  
  ## Structural error
  #error <- rnorm(n)
  
  #error <- rcauchy(n)
  
  #error <- rt(n, 3)
  
  # Indicator for contaminated observations
  contam <- rbinom(n, 1, 0.20)
  
  # Generate contaminated errors
  error <- ifelse(contam == 0,
                  rnorm(n, 0, 1),
                  rnorm(n, 0, 5))
  
  #Laplace distribution
  #error<-rLaplace(n)
  
  # Heteroskedastic error where variance depends on x1 and x2
  #sigma_2 <- sqrt(abs(beta[1] + beta[2]*x_1 + beta[3]*x_2 + beta[4]*e_1))
  #error <- rnorm(n, mean = 0, sd = sigma_2)
  
  ##-----------------------------------------------------------
  ## Latent response + left-censoring at zero
  ##-----------------------------------------------------------
  y_star <- beta[1] + beta[2]*x_1 + beta[3]*x_2 + beta[4] * e_1 + error
  c_fixed <- 0
  y<- pmax(c_fixed, y_star)
  # 1 = uncensored, 0 = censored
  event  <- as.numeric(y_star > c_fixed)   
  
  c_p <-(sum(y==c_fixed)/length(y))*100
  
  dat <- data.frame(y, x_1, x_2, res, event)
  
  dat$yc <- rep(0, nrow(dat))
  
  ##-----------------------------------------------------------
  ## STEP 1: Fit CLAD (censored quantile / Powell estimator) -> beta_hat
  ##-----------------------------------------------------------
  fit <- crq(Curv(y, yc, ctype = "left") ~ x_1 + x_2 + res,
             data = dat, method = "Pow", tau = 0.5)
  beta_hat <- coef(fit)
  
  Xmat        <- model.matrix(~ x_1 + x_2 + res, data = dat)
  fitted_vals <- as.vector(Xmat %*% beta_hat)
  
  ##-----------------------------------------------------------
  ## STEP 2: For UNCENSORED obs -> exact true residuals
  ## STEP 3: For CENSORED obs   -> only the inequality y_i* <= 0
  ##          => e_i <= r_i  (r_i computed using observed y_i, which is 0)
  ##          This inequality is only informative for |e_i| when r_i <= 0.
  ##          Censored points with r_i > 0 carry no usable bound and
  ##          must be excluded -- this is NOT "ignoring censored data",
  ##          it is exactly what the algorithm means by "providing only
  ##          the inequality" (i.e. some censored points are uninformative).
  ##-----------------------------------------------------------
  r_i <- dat$y - fitted_vals
  
  keep       <- !(dat$event == 0 & r_i > 0)
  n_excluded <- sum(!keep)
  
  abs_r   <- abs(r_i[keep])
  event_k <- dat$event[keep]     # 1 = exact residual, 0 = right-censored |e_i|
  
  ##-----------------------------------------------------------
  ## STEP 4: Robust estimator of residual distribution
  ##          = uncensored exact residuals + censoring info (KM)
  ##-----------------------------------------------------------
  km_fit <- survfit(Surv(abs_r, event_k) ~ 1)
  
  ##-----------------------------------------------------------
  ## STEP 5: Quantiles of the estimated residual distribution
  ##-----------------------------------------------------------
  tau_levels <- c(0.50, 0.75, 0.85)
  Q <- quantile(km_fit, probs = tau_levels)$quantile
  
  Q50 <- unname(Q[1])
  Q75 <- unname(Q[2])
  Q85 <- unname(Q[3])
  
  Huber_c <- Q85
  Hampel_a <- Q50
  Hampel_b <- Q75
  Hampel_c <- Q85
  Tukey_c <- Q85
  
  ##-----------------------------------------------------------
  ## Return results
  ##-----------------------------------------------------------
  return(c(
    beta_hat   = beta_hat,
    Huber_c    = Huber_c,
    Hampel_a   = Hampel_a,
    Hampel_b   = Hampel_b,
    Hampel_c   = Hampel_c,
    Tukey_c    = Tukey_c,
    c_p = c_p
  ))
}


### initial parameter
set.seed(123)
mle_par <- function(beta, n) {
  
  ## Instruments and regressors
  z   <- runif(n)
  e_1 <- rnorm(n)
  x_1 <- rnorm(n)
  
  ## Endogenous regressor
  x_2 <- beta[1] + beta[2]*x_1 + beta[3]*z + e_1
  
  ## First-stage residuals
  res <- residuals(lm(x_2 ~ z + x_1))
  
  
  ## Latent response
  y_star <- beta[1] + beta[2]*x_1 + beta[3]*x_2 + beta[4] * e_1 + rnorm(n)
  
  ## Left-censored response
  y <- pmax(0, y_star)
  
  ## Data
  data <- data.frame(y, x_1, x_2, res)
  
  ## Tobit MLE
  mle_fit <- tobit(y ~ x_1 + x_2 + res,left = 0, data = data,control = survreg.control(maxiter = 1000))
  
  ## Return both estimates and data
  return(as.numeric(coef(mle_fit)))
}

### clad loss
set.seed(123)
f_1_clad<- function(beta, n) {
  
  ## Instruments and regressors
  z   <- runif(n)
  e_1 <- rnorm(n)
  x_1 <- rnorm(n)
  
  ## Endogenous regressor
  x_2 <- beta[1] + beta[2]*x_1 + beta[3]*z + e_1
  
  # =========================
  # Stage 1: LSE
  # =========================
  res <- residuals(lm(x_2 ~ z + x_1))
  
  ## Structural error
  
  ## Standard Normal
  #error <- rnorm(n)
  
  # Cauchy distribution
  #error <-  rcauchy(n)
  
  ## Student t Distribution
   #error <- rt(n,3)
  
  # Indicator for contaminated observations
  contam <- rbinom(n, 1, 0.20)
  
  # Generate contaminated errors
  error <- ifelse(contam == 0,
                  rnorm(n, 0, 1),
                  rnorm(n, 0, 5))
  
  #Laplace distribution
  #error<-rLaplace(n)
  
  # Heteroskedastic error where variance depends on x1 and x2
  #sigma_2 <- sqrt(abs(beta[1] + beta[2]*x_1 + beta[3]*x_2 + beta[4]*e_1))
  
  #error <- rnorm(n, mean = 0, sd = sigma_2)
  
  ## Latent variable
  y_star<- beta[1] + beta[2]*x_1 + beta[3]*x_2  + beta[4]*e_1 + error
  
  ## Censored outcome
  y <- pmax(0, y_star)
  cp_d<-(sum(y==0)/length(y))*100
  
  
  data <- data.frame(y, x_1, x_2, res)
  
  # =========================
  # Stage 2: Sclsloss
  # =========================
  
  f_d <- function(theta) {
    mu_hat <- theta[1] + theta[2]*x_1 + theta[3]*x_2 + theta[4]*res
    r <- y - pmax(0, mu_hat)
    mean(quantile_loss(r))
  }
  
  # Use MLE as initial values
  mle_p<-mle_par(beta,n)
  init <- as.numeric(mle_p)
  
  clad_fit <- optim(init, f_d, method = "Nelder-Mead")
  
  c(clad_fit$par, cp_d)
}


### Scls loss
set.seed(123)
f_1_scls<- function(beta, n) {
  
  ## Instruments and regressors
  z   <- runif(n)
  e_1 <- rnorm(n)
  x_1 <- rnorm(n)
  
  ## Endogenous regressor
  x_2 <- beta[1] + beta[2]*x_1 + beta[3]*z + e_1
  
  # =========================
  # Stage 1: LSE
  # =========================
  res <- residuals(lm(x_2 ~ z + x_1))
  
  ## Structural error
  
  ## Standard Normal
  #error <- rnorm(n)
  
  # Cauchy distribution
  #error <-  rcauchy(n)
  
  #Laplace distribution
  #error<-rLaplace(n)
  
  ## Student t Distribution
   #error <- rt(n,3)
  
  # Indicator for contaminated observations
  contam <- rbinom(n, 1, 0.20)
  
  # Generate contaminated errors
  error <- ifelse(contam == 0,
                  rnorm(n, 0, 1),
                  rnorm(n, 0, 5))
  
  # Heteroskedastic error where variance depends on x1 and x2
  #sigma_2 <- sqrt(abs(beta[1] + beta[2]*x_1 + beta[3]*x_2 + beta[4]*e_1))
  
  #error <- rnorm(n, mean = 0, sd = sigma_2)
  
  ## Latent variable
  y_star<- beta[1] + beta[2]*x_1 + beta[3]*x_2  + beta[4]*e_1 + error
  
  ## Censored outcome
  y <- pmax(0, y_star)
  cp_s<-(sum(y==0)/length(y))*100
  
  
  data <- data.frame(y, x_1, x_2, res)
  Xmat <- cbind(1, x_1, x_2, res)
  
  # =========================
  # Stage 2: Sclsloss
  # =========================
  
  f_s <- function(theta) {
    mean(scls_loss(theta, y, Xmat))
  }
  
  # Use MLE as initial values
  mle_p<-mle_par(beta,n)
  init <- as.numeric(mle_p)
  
  scls_fit <- optim(init, f_s, method = "Nelder-Mead")
  
  c(scls_fit$par, cp_s)
}


### Huber loss
set.seed(123)
f_1_huber<- function(beta, n) {
  
  ## Instruments and regressors
  z   <- runif(n)
  e_1 <- rnorm(n)
  x_1 <- rnorm(n)
  
  ## Endogenous regressor
  x_2 <- beta[1] + beta[2]*x_1 + beta[3]*z + e_1
  
  # =========================
  # Stage 1: LSE
  # =========================
  res <- residuals(lm(x_2 ~ z + x_1))
  
  ## Structural error
  
  ## Standard Normal
  #error <- rnorm(n)
  
  # Cauchy distribution
  #error <-  rcauchy(n)
  
  ## Student t Distribution
   #error <- rt(n,3)
  
  # Indicator for contaminated observations
  contam <- rbinom(n, 1, 0.20)
  
  # Generate contaminated errors
  error <- ifelse(contam == 0,
                  rnorm(n, 0, 1),
                  rnorm(n, 0, 5))
  
  #Laplace distribution
  #error<-rLaplace(n)
  
  # Heteroskedastic error where variance depends on x1 and x2
  #sigma_2 <- sqrt(abs(beta[1] + beta[2]*x_1 + beta[3]*x_2+beta[4]*e_1))
  
  #error <- rnorm(n, mean = 0, sd = sigma_2)
  
  ## Latent variable
  y_star <- beta[1] + beta[2]*x_1 + beta[3]*x_2  + beta[4] * (e_1) + error
  
  ## Censored outcome
  y <- pmax(0, y_star)
  
  cp_h<-(sum(y==0)/length(y))*100
  
  data <- data.frame(y, x_1, x_2, res)
  out<-Tun_cons(beta,n)
  
  # =========================
  # Stage 2: Huber Tobit (CF)
  # =========================
  
  f2 <- function(theta) {
    mu_hat <- theta[1] + theta[2]*x_1 + theta[3]*x_2 + theta[4]*res
    r <- y - pmax(out["Huber_c"], mu_hat)
    mean(huber_loss(r,out["Huber_c"]))
  }
  
  # Use MLE as initial values
  mle_p<-mle_par(beta,n)
  init <- as.numeric(mle_p)
  
  huber_fit <- optim(init, f2, method = "Nelder-Mead")
  
  c(huber_fit$par, cp_h)
}
f_1_huber(c(1,2,3,0.5),1000)

### Hampel loss optimization
set.seed(123)
f_1_hampel<-function(beta, n) {
  
  ## Instruments and regressors
  z   <- runif(n)
  e_1 <- rnorm(n)
  x_1 <- rnorm(n)
  
  ## Endogenous regressor
  x_2 <- beta[1] + beta[2]*x_1 + beta[3]*z + e_1
  
  # =========================
  # Stage 1: LSE
  # =========================
  res <- residuals(lm(x_2 ~ z + x_1))
  
  ## Structural error
  
  ## Standard Normal
  #error <- rnorm(n)
  
  # Cauchy distribution
  #error <-rcauchy(n)
  
  ## Student t Distribution
    #error <- rt(n,3)
    
  # Indicator for contaminated observations
  contam <- rbinom(n, 1, 0.20)
  
  # Generate contaminated errors
  error <- ifelse(contam == 0,
                  rnorm(n, 0, 1),
                  rnorm(n, 0, 5))
  
  #Laplace distribution
  #error<-rLaplace(n)
  
  # Heteroskedastic error where variance depends on x1 and x2
  #sigma_2 <- sqrt(abs(beta[1] + beta[2]*x_1 + beta[3]*x_2+beta[4]*e_1))
  
  #error <- rnorm(n, mean = 0, sd = sigma_2)
  
  ## Latent variable
  y_star <- beta[1] + beta[2]*x_1 + beta[3]*x_2 + beta[4] *e_1 + error
  
  ## Censored outcome
  y <- pmax(0, y_star)
  
  cp_hm <-(sum(y==0)/length(y))*100
  
  data <- data.frame(y, x_1, x_2, res)
  
  #tunning constant
  out<-Tun_cons(beta,n)
  
  # =========================
  # Hample Tobit
  # =========================
  
  f3 <- function(theta) {
    mu_hat <- theta[1] + theta[2]*x_1 + theta[3]*x_2 + theta[4]*res
    r <- y - pmax(out[("Hampel_c")], mu_hat)
    mean(hampel_loss(r,a = out["Hampel_a"], b = out["Hampel_b"], c = out["Hampel_c"]))
  }
  
  # Use MLE as initial values
  mle_p<-mle_par(beta,n)
  init <- as.numeric(mle_p)
  
  fit <- optim(init, f3, method = "Nelder-Mead")
  
  c(fit$par, cp_hm)
}


### Tukey loss optimization
set.seed(123)
f_1_tukey<- function(beta, n) {
  
  ## Instruments and regressors
  z   <- runif(n)
  e_1 <- rnorm(n)
  x_1 <- rnorm(n)
  
  ## Endogenous regressor
  x_2 <- beta[1] + beta[2]*x_1 + beta[3]*z + e_1
  
  # =========================
  # Stage 1: LSE
  # =========================
  res <- residuals(lm(x_2 ~ z + x_1))
  
  ## Structural error
  
  ## Standard Normal
  #error <- rnorm(n)
  
  # Cauchy distribution
  #error <-rcauchy(n)
  
  ## Student t Distribution
  #error <- rt(n,3)
  
  # Indicator for contaminated observations
  contam <- rbinom(n, 1, 0.20)
  
  # Generate contaminated errors
  error <- ifelse(contam == 0,
                  rnorm(n, 0, 1),
                  rnorm(n, 0, 5))
  
  #Laplace distribution
  #error<-rLaplace(n)
  
  # Heteroskedastic error where variance depends on x1 and x2
  #sigma_2 <- sqrt(abs(beta[1] + beta[2]*x_1 + beta[3]*x_2+beta[4]*e_1))
  
  #error <- rnorm(n, mean = 0, sd = sigma_2)
  
  ## Latent variable
  y_star <- beta[1] + beta[2]*x_1 + beta[3]*x_2 + beta[4] *e_1 + error
  
  ## Censored outcome
  y <- pmax(0, y_star)
  
  cp_t<-(sum(y==0)/length(y))*100
  
  data <- data.frame(y, x_1, x_2, res)
  
  ##Tunning constant
  out<-Tun_cons(beta,n)
  
  # =========================
  # Tukey Tobit
  # =========================
  
  f4 <- function(theta) {
    mu_hat <- theta[1] + theta[2]*x_1 + theta[3]*x_2 + theta[4]*res
    r <- y - pmax(out["Tukey_c"], mu_hat)
    mean(tukey_loss(r,out["Tukey_c"]))
  }
  
  # Use MLE as initial values
  mle_p<-mle_par(beta,n)
  init <- as.numeric(mle_p)
  
  fit <- optim(init, f4, method = "Nelder-Mead")
  
  c(fit$par, cp_t)
}


# Monte Carlo parameters
r    <- 2000
beta <- c(1, 2, 3, 0.5)

# Export necessary objects to cluster
clusterExport(cl, c("huber_loss", "hampel_loss", "tukey_loss", "scls_loss",
                    "Tun_cons", "mle_par",
                    "f_1_huber", "f_1_hampel", "f_1_tukey", "f_1_scls","f_1_clad",
                    "beta", "r"))

clusterEvalQ(cl, {
  library(MASS)
  library(ExtDist)
  library(AER)
  library(quantreg)
  library(survival)
})

cat("Running parallel Monte Carlo simulations...\n")
cat("Using", n_cores, "cores\n\n")

# ---------------------------------------------------------------------
# Helper: given a 4 x r matrix of beta_hat draws and the true beta vector,
# returns a named vector of Mean Bias, MSE, Median Bias, MAE, MedAE
# for each of the 4 coefficients.
# ---------------------------------------------------------------------
compute_metrics <- function(beta_hat, beta, prefix) {
  err <- beta_hat - beta          # 4 x r matrix of errors (signed)
  abs_err <- abs(err)
  
  out <- c()
  for (j in 1:4) {
    out <- c(out,
             mean(err[j, ]),            # Mean Bias
             mean(err[j, ]^2),          # MSE
             median(err[j, ]),          # Median Bias
             mean(abs_err[j, ]),        # MAE
             median(abs_err[j, ])       # MedAE
    )
  }
  names(out) <- as.vector(sapply(1:4, function(j)
    paste0(c("MeanBias_", "MSE_", "MedBias_", "MAE_", "MedAE_"), prefix, j)))
  out
}

## Parallel SCLS estimation
cat("Running scls estimator...\n")
results_scls <- foreach(N = sample_size, .combine = 'rbind',
                        .packages = c('MASS', 'ExtDist', 'AER', 'quantreg', 'survival')) %dopar% {
                          set.seed(123 + N)
                          sims <- replicate(r, f_1_scls(beta, N))
                          beta_hat <- sims[1:4, ]
                          c(compute_metrics(beta_hat, beta, "s"), cp_s = sims[5, 1])
                        }

## Parallel CLAD estimation
cat("Running Clad estimator...\n")
results_clad <- foreach(N = sample_size, .combine = 'rbind',
                        .packages = c('MASS', 'ExtDist', 'AER', 'quantreg', 'survival')) %dopar% {
                          set.seed(123 + N)
                          sims <- replicate(r, f_1_clad(beta, N))
                          beta_hat <- sims[1:4, ]
                          c(compute_metrics(beta_hat, beta, "c"), cp_d = sims[5, 1])
                        }

# Parallel WME (Huber) estimation
cat("Running WME estimator...\n")
results_WME <- foreach(N = sample_size, .combine = 'rbind',
                       .packages = c('MASS', 'ExtDist', 'AER', 'quantreg', 'survival')) %dopar% {
                         set.seed(123 + N)
                         sims <- replicate(r, f_1_huber(beta, N))
                         beta_hat <- sims[1:4, ]
                         c(compute_metrics(beta_hat, beta, "W"), cp_h = sims[5, 1])
                       }

# Parallel Hampel estimation
cat("Running hampel estimator...\n")
results_hampel <- foreach(N = sample_size, .combine = 'rbind',
                          .packages = c('MASS', 'ExtDist', 'AER', 'quantreg', 'survival')) %dopar% {
                            set.seed(123 + N)
                            sims <- replicate(r, f_1_hampel(beta, N))
                            beta_hat <- sims[1:4, ]
                            c(compute_metrics(beta_hat, beta, "h"), cp_hm = sims[5, 1])
                          }

# Parallel Tukey estimation
cat("Running tukey estimator...\n")
results_tukey <- foreach(N = sample_size, .combine = 'rbind',
                         .packages = c('MASS', 'ExtDist', 'AER', 'quantreg', 'survival')) %dopar% {
                           set.seed(123 + N)
                           sims <- replicate(r, f_1_tukey(beta, N))
                           beta_hat <- sims[1:4, ]
                           c(compute_metrics(beta_hat, beta, "T"), cp_t = sims[5, 1])
                         }

# Stop cluster
stopCluster(cl)

cat("\nSimulations complete!\n\n")

# ---------------------------------------------------------------------
# Extraction helper: pulls the 5 metrics x 4 betas out of a results
# matrix for a given prefix, plus censoring %, into a data.frame ready
# to print/plot.
# ---------------------------------------------------------------------
extract_metrics <- function(results, prefix, cp_name) {
  df <- data.frame(
    sapply(1:4, function(j) results[, paste0("MeanBias_", prefix, j)]),
    sapply(1:4, function(j) results[, paste0("MSE_", prefix, j)]),
    sapply(1:4, function(j) results[, paste0("MedBias_", prefix, j)]),
    sapply(1:4, function(j) results[, paste0("MAE_", prefix, j)]),
    sapply(1:4, function(j) results[, paste0("MedAE_", prefix, j)])
  )
  colnames(df) <- as.vector(sapply(c("MeanBias", "MSE", "MedBias", "MAE", "MedAE"),
                                   function(m) paste0(m, "_", prefix, 1:4)))
  df$Censoring_Percent <- results[, cp_name]
  df
}

metrics_scls   <- extract_metrics(results_scls,   "s", "cp_s")
metrics_clad   <- extract_metrics(results_clad,   "c", "cp_d")
metrics_WME    <- extract_metrics(results_WME,    "W", "cp_h")
metrics_hampel <- extract_metrics(results_hampel, "h", "cp_hm")
metrics_tukey  <- extract_metrics(results_tukey,  "T", "cp_t")

# Indices to display
#idx <- c(1, 2, 10, 19, 20)
idx <- seq(1,20,1)

# ---- CLAD Results ----
cat("CLAD Results:\n")
print(round(data.frame(n = sample_size[idx], metrics_clad[idx, ]), 4))

# ---- SCLS Results ----
cat("\nscls Results:\n")
print(round(data.frame(n = sample_size[idx], metrics_scls[idx, ]), 4))

# ---- WME (Huber) Results ----
cat("\nWME Results:\n")
print(round(data.frame(n = sample_size[idx], metrics_WME[idx, ]), 4))

# ---- Hampel Results ----
cat("\nHampel Results:\n")
print(round(data.frame(n = sample_size[idx], metrics_hampel[idx, ]), 4))

# ---- Tukey Results ----
cat("\nTukey Results:\n")
print(round(data.frame(n = sample_size[idx], metrics_tukey[idx, ]), 4))


# ---------------------------------------------------------------------
# Plotting: RMSE of beta_0..beta_3 for all five estimators
# (swap "RMSE" for "MeanBias", "MedBias", "MAE", or "MedAE" below to
#  plot a different metric)
# ---------------------------------------------------------------------
par(mfrow = c(2, 2))

plot_metric <- function(metric_list, main, ylab) {
  cols <- c("red", "blue", "green4", "orange", "purple")
  plot(sample_size, metric_list[[1]], type = "o", col = cols[1], pch = 21,
       xlab = "Sample Size", ylab = ylab, main = main,
       ylim = range(unlist(metric_list)))
  for (i in 2:length(metric_list)) {
    lines(sample_size, metric_list[[i]], col = cols[i], type = "o", pch = 20 + i)
  }
  abline(h = 0, col = "black", lty = 2)
  legend("topright", c("CLAD", "SCLS", "Huber", "Hampel", "Tukey"),
         col = cols, lty = 1, cex = 0.7)
}

metric_name <- "MSE"  # change to "MeanBias", "MedBias", "MAE", or "MedAE" as needed

for (j in 1:4) {
  plot_metric(
    list(metrics_clad[[paste0(metric_name, "_c", j)]],
         metrics_scls[[paste0(metric_name, "_s", j)]],
         metrics_WME[[paste0(metric_name, "_W", j)]],
         metrics_hampel[[paste0(metric_name, "_h", j)]],
         metrics_tukey[[paste0(metric_name, "_T", j)]]),
    main = paste0(metric_name, " of Beta_", j - 1),
    ylab = metric_name
  )
}
