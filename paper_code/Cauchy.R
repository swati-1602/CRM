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

# ---------------------------------------------------------------------
# Parallel setup
# ---------------------------------------------------------------------

n_cores <- max(1, detectCores() - 1)
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Sample sizes
#sample_size <- c(100, 200)
sample_size <- seq(100, 1000, 100)

# ---------------------------------------------------------------------
# Loss functions
# ---------------------------------------------------------------------

huber_loss <- function(r, c) {
  ifelse(
    abs(r) <= c,
    0.5 * r^2,
    c * (abs(r) - 0.5 * c)
  )
}

hampel_loss <- function(r, a, b, c) {
  x <- abs(r)
  rho <- numeric(length(x))
  
  # |r| < a : quadratic
  idx1 <- x < a
  rho[idx1] <- 0.5 * x[idx1]^2
  
  # a <= |r| < b : linear
  idx2 <- x >= a & x < b
  rho[idx2] <- a * (x[idx2] - a / 2)
  
  # b <= |r| < c : redescending part
  idx3 <- x >= b & x < c
  rho[idx3] <- a * (b - a / 2) +
    a * ((c - b) / 2 - (c - x[idx3])^2 / (2 * (c - b)))
  
  # |r| >= c : constant
  idx4 <- x >= c
  rho[idx4] <- a * (b + c - a) / 2
  
  rho
}

tukey_loss <- function(r, c) {
  u <- r / c
  
  ifelse(
    abs(u) <= 1,
    (c^2 / 6) * (1 - (1 - u^2)^3),
    c^2 / 6
  )
}

# FIX: tau = 0.75
quantile_loss <- function(r, tau = 0.75) {
  ifelse(r >= 0, tau * r, (tau - 1) * r)
}

# FIX: second term is ADDED (Powell SCLS), not subtracted
scls_loss <- function(beta, y, x) {
  eta <- as.vector(x %*% beta)
  
  part1 <- (y - pmax(0.5 * y, eta))^2
  
  part2 <- ifelse(
    y > 2 * eta,
    (0.5 * y)^2 - pmax(0, eta)^2,
    0
  )
  
  sum(part1 + part2)
}

# ---------------------------------------------------------------------
# Common data-generating process
#
# Structural model:
# y*_i = b0 + b1 x1_i + b2 x2_i + b3 v_i + eps_i
# y_i  = max(0, y*_i)
#
# First stage:
# x2_i = d0 + d1 x1_i + d2 z_i + v_i
#
# Control function:
# res_i = residual from lm(x2 ~ x1 + z)
# ---------------------------------------------------------------------
generate_data <- function(beta, delta, n) {
  
  ##instrumental variable
  z_1 <- runif(n)
  
  ## exogenous regressor
  x_1 <- rnorm(n)
  
  ## first stage error
  v_1 <- rnorm(n)
  
  ## structural error
  epsilon <- rcauchy(n) 
  
  ### endogenous regressor
  x_2 <- delta[1] + delta[2] * x_1 + delta[3] * z_1 + v_1
  
  ## first stage residuals
  first_stage <- lm(x_2 ~ x_1 + z_1)
  res <- residuals(first_stage)
  
  ##latent variable 
  y_star <- beta[1] + beta[2] * x_1 +beta[3] * x_2 + beta[4] * v_1 + epsilon
  
  ## response variable 
  y <- pmax(0, y_star)
  
  list(
    y = y,
    x_1 = x_1,
    x_2 = x_2,
    z_1 = z_1,
    v_1 = v_1,
    res = res,
    y_star = y_star
  )
}

# ---------------------------------------------------------------------
# Tuning constants
# FIX: now uses the same data d as the estimator (no new data generated)
# ---------------------------------------------------------------------
Tun_cons <- function(d) {
  
  # Initial CLAD/control-function fit
  y <- d$y
  X <- cbind(1, d$x_1, d$x_2, d$res)
  
  dat <- data.frame(y = y, x_1 = d$x_1, x_2 = d$x_2, res = d$res)
  
  # Uncensored observations: y > 0
  dat$event <- as.numeric(dat$y > 0)
  
  # Initial LAD/control-function fit on uncensored observations
  unc <- dat[dat$y > 0, ]
  
  # LAD start on uncensored observations only
  start <- as.numeric(coef(
    rq(y ~ x_1 + x_2 + res, tau = 0.5, data = unc)
  ))
  
  # Powell censored LAD objective
  obj <- function(b) mean(abs(y - pmax(0, as.vector(X %*% b))))
  
  fit <- optim(start, obj, method = "Nelder-Mead",
               control = list(maxit = 5000))
  
  
  beta_hat <- fit$par
  
  Xmat <- model.matrix(~ x_1 + x_2 + res, data = dat)
  fitted_vals <- as.vector(Xmat %*% beta_hat)
  
  neg_r <- fitted_vals - dat$y
  shift <- -min(neg_r) + 1
  
  km_fit <- survfit(Surv(neg_r + shift, event) ~ 1, data = dat)
  
  last_event <- if (any(km_fit$n.event > 0)) {
    max(km_fit$time[km_fit$n.event > 0])
  } else {
    max(km_fit$time)
  }
  
  Qfun <- function(p) {
    q <- tryCatch(
      unname(quantile(km_fit, probs = p)$quantile),
      error = function(e) NA_real_
    )
    
    if (is.na(q)) q <- last_event
    
    q - shift
  }
  
  c_tau <- function(tau) {
    max(
      Qfun((1 + tau) / 2),
      -Qfun((1 - tau) / 2),
      1e-6
    )
  }
  
  Q65 <- c_tau(0.65)
  Q75 <- c_tau(0.75)
  Q85 <- c_tau(0.85)
  
  c(
    Huber_c  = Q65,
    Hampel_a = Q65,
    Hampel_b = Q75,
    Hampel_c = Q85,
    Tukey_c  = Q85
  )
}
# ---------------------------------------------------------------------
# Initial Tobit/control-function estimator
# FIX: now uses the same data d as the estimator (no new data generated)
# ---------------------------------------------------------------------
 clad_par <- function(d) {
   y <- d$y
  X <- cbind(1, d$x_1, d$x_2, d$res)
  
  dat <- data.frame(y = y, x_1 = d$x_1, x_2 = d$x_2, res = d$res)
  
  # LAD start on uncensored observations only
  start <- as.numeric(coef(
    rq(y ~ x_1 + x_2 + res, tau = 0.5, data = dat, subset = y > 0)
  ))
  
  # Powell censored LAD objective
  obj <- function(b) mean(abs(y - pmax(0, as.vector(X %*% b))))
  
  fit <- optim(start, obj, method = "Nelder-Mead",
               control = list(maxit = 5000))
  
  fit$par
}


# ---------------------------------------------------------------------
# Quantile-loss estimator
# ---------------------------------------------------------------------

f_1_quantile <- function(beta, delta, n) {
  
  d <- generate_data(beta, delta, n)
  
  y <- d$y
  x_1 <- d$x_1
  x_2 <- d$x_2
  res <- d$res
  
  cp <- mean(y == 0) * 100
  
  f_d <- function(theta) {
    mu_hat <- pmax(
      0,
      theta[1] + theta[2] * x_1 +theta[3] * x_2 + theta[4] * res
    )
    
    r <- y - mu_hat
    
    mean(quantile_loss(r))
  }
  
  init <- clad_par(d)
  
  fit <- optim(
    init,
    f_d,
    method = "Nelder-Mead",
    control = list(maxit = 5000)
  )
  
  c(fit$par, cp)
}

# ---------------------------------------------------------------------
# SCLS estimator
# ---------------------------------------------------------------------

f_1_scls <- function(beta, delta, n) {
  
  d <- generate_data(beta, delta, n)
  
  y <- d$y
  x_1 <- d$x_1
  x_2 <- d$x_2
  res <- d$res
  
  cp <- mean(y == 0) * 100
  
  Xmat <- cbind(1, x_1, x_2, res)
  
  f_s <- function(theta) {
    mean(scls_loss(theta, y, Xmat))
  }
  
  init <- clad_par(d)
  
  fit <- optim(
    init,
    f_s,
    method = "Nelder-Mead",
    control = list(maxit = 5000)
  )
  
  c(fit$par, cp)
}


# ---------------------------------------------------------------------
# Huber-loss estimator
# ---------------------------------------------------------------------

f_1_huber <- function(beta, delta, n) {
  
  d <- generate_data(beta, delta, n)
  
  y <- d$y
  x_1 <- d$x_1
  x_2 <- d$x_2
  res <- d$res
  
  cp <- mean(y == 0) * 100
  
  tun <- Tun_cons(d)
  c_h <- tun["Huber_c"]
  
  f2 <- function(theta) {
    mu_hat <- theta[1] + theta[2] * x_1 +theta[3] * x_2 + theta[4] * res
    
    # FIX: censoring point is 0, not the tuning constant
    r <- y - pmax(c_h, mu_hat)
    
    mean(huber_loss(r, c_h))
  }
  
  # FIX: mle_par() did not exist, use clad_par()
  init <- clad_par(d)
  
  fit <- optim(
    init,
    f2,
    method = "Nelder-Mead",
    control = list(maxit = 5000)
  )
  
  c(fit$par, cp)
}

# ---------------------------------------------------------------------
# Hampel-loss estimator
# ---------------------------------------------------------------------

f_1_hampel <- function(beta, delta, n) {
  
  d <- generate_data(beta, delta, n)
  
  y <- d$y
  x_1 <- d$x_1
  x_2 <- d$x_2
  res <- d$res
  
  cp <- mean(y == 0) * 100
  
  tun <- Tun_cons(d)
  
  a_h <- tun["Hampel_a"]
  b_h <- tun["Hampel_b"]
  c_h <- tun["Hampel_c"]
  
  f3 <- function(theta) {
    mu_hat <- theta[1] + theta[2] * x_1 +theta[3] * x_2 + theta[4] * res
    
    # FIX: censoring point is 0, not the tuning constant
    r <- y - pmax(c_h, mu_hat)
    
    mean(
      hampel_loss(
        r,
        a = a_h,
        b = b_h,
        c = c_h
      )
    )
  }
  
  # FIX: mle_par() did not exist, use clad_par()
  init <- clad_par(d)
  
  fit <- optim(
    init,
    f3,
    method = "Nelder-Mead",
    control = list(maxit = 5000)
  )
  
  c(fit$par, cp)
}


# ---------------------------------------------------------------------
# Tukey-loss estimator
# ---------------------------------------------------------------------

f_1_tukey <- function(beta, delta, n) {
  
  d <- generate_data(beta, delta, n)
  
  y <- d$y
  x_1 <- d$x_1
  x_2 <- d$x_2
  res <- d$res
  
  cp <- mean(y == 0) * 100
  
  tun <- Tun_cons(d)
  c_t <- tun["Tukey_c"]
  
  f4 <- function(theta) {
    mu_hat <- theta[1] + theta[2] * x_1 +theta[3] * x_2 + theta[4] * res
    
    # FIX: censoring point is 0, not the tuning constant
    r <- y - pmax(c_t, mu_hat)
    
    mean(tukey_loss(r, c_t))
  }
  
  # FIX: mle_par() did not exist, use clad_par()
  init <- clad_par(d)
  
  fit <- optim(
    init,
    f4,
    method = "Nelder-Mead",
    control = list(maxit = 5000)
  )
  
  c(fit$par, cp)
}


# ---------------------------------------------------------------------
# Monte Carlo settings
# ---------------------------------------------------------------------

r <- 2000

beta <- c(1, 2, 3, 0.5)
delta <- c(1, 2, 3)

n_coef <- length(beta)

# ---------------------------------------------------------------------
# Metrics
# (moved above clusterExport so it can be exported to the workers)
# ---------------------------------------------------------------------

compute_metrics <- function(beta_hat, beta, prefix) {
  
  err <- beta_hat - beta
  abs_err <- abs(err)
  
  out <- c()
  
  for (j in seq_len(n_coef)) {
    out <- c(
      out,
      mean(err[j, ]),
      mean(err[j, ]^2),
      median(err[j, ]),
      mean(abs_err[j, ]),
      median(abs_err[j, ])
    )
  }
  
  names(out) <- as.vector(
    sapply(
      seq_len(n_coef),
      function(j) {
        paste0(
          c("MeanBias_", "MSE_", "MedBias_", "MAE_", "MedAE_"),
          prefix,
          j
        )
      }
    )
  )
  
  out
}

clusterExport(
  cl,
  c(
    "huber_loss",
    "hampel_loss",
    "tukey_loss",
    "quantile_loss",
    "scls_loss",
    "generate_data",
    "Tun_cons",
    "clad_par",
    "f_1_quantile",
    "f_1_scls",
    "f_1_huber",
    "f_1_hampel",
    "f_1_tukey",
    "compute_metrics",
    "beta",
    "delta",
    "r",
    "n_coef"
    
  )
)

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
# Parallel simulations
# set.seed(123 + N) inside each block is kept: every estimator sees the
# same simulated datasets for a given sample size N.
# ---------------------------------------------------------------------

cat("Running SCLS estimator...\n")

results_scls <- foreach(
  N = sample_size,
  .combine = "rbind",
  .packages = c("MASS", "ExtDist", "AER", "quantreg", "survival")
) %dopar% {
  
  set.seed(123 + N)
  
  sims <- replicate(r, f_1_scls(beta, delta, N))
  
  beta_hat <- sims[1:n_coef, ]
  cp <- mean(sims[n_coef + 1, ])
  
  c(
    compute_metrics(beta_hat, beta, "s"),
    cp_s = cp
  )
}

cat("Running quantile estimator...\n")

results_quantile <- foreach(
  N = sample_size,
  .combine = "rbind",
  .packages = c("MASS", "ExtDist", "AER", "quantreg", "survival")
) %dopar% {
  
  set.seed(123 + N)
  
  sims <- replicate(r, f_1_quantile(beta, delta, N))
  
  beta_hat <- sims[1:n_coef, ]
  cp <- mean(sims[n_coef + 1, ])
  
  c(
    compute_metrics(beta_hat, beta, "q"),
    cp_q = cp
  )
}

cat("Running Huber estimator...\n")

results_WME <- foreach(
  N = sample_size,
  .combine = "rbind",
  .packages = c("MASS", "ExtDist", "AER", "quantreg", "survival")
) %dopar% {
  
  set.seed(123 + N)
  
  sims <- replicate(r, f_1_huber(beta, delta, N))
  
  beta_hat <- sims[1:n_coef, ]
  cp <- mean(sims[n_coef + 1, ])
  
  c(
    compute_metrics(beta_hat, beta, "W"),
    cp_h = cp
  )
}

cat("Running Hampel estimator...\n")

results_hampel <- foreach(
  N = sample_size,
  .combine = "rbind",
  .packages = c("MASS", "ExtDist", "AER", "quantreg", "survival")
) %dopar% {
  
  set.seed(123 + N)
  
  sims <- replicate(r, f_1_hampel(beta, delta, N))
  
  beta_hat <- sims[1:n_coef, ]
  cp <- mean(sims[n_coef + 1, ])
  
  c(
    compute_metrics(beta_hat, beta, "h"),
    cp_hm = cp
  )
}

cat("Running Tukey estimator...\n")

results_tukey <- foreach(
  N = sample_size,
  .combine = "rbind",
  .packages = c("MASS", "ExtDist", "AER", "quantreg", "survival")
) %dopar% {
  
  set.seed(123 + N)
  
  sims <- replicate(r, f_1_tukey(beta, delta, N))
  
  beta_hat <- sims[1:n_coef, ]
  cp <- mean(sims[n_coef + 1, ])
  
  c(
    compute_metrics(beta_hat, beta, "T"),
    cp_t = cp
  )
}

stopCluster(cl)

cat("\nSimulations complete!\n\n")

# ---------------------------------------------------------------------
# Extract metrics
# ---------------------------------------------------------------------

extract_metrics <- function(results, prefix, cp_name) {
  
  metric_names <- c(
    "MeanBias",
    "MSE",
    "MedBias",
    "MAE",
    "MedAE"
  )
  
  df <- data.frame(
    sapply(seq_len(n_coef), function(j) {
      results[, paste0("MeanBias_", prefix, j)]
    }),
    sapply(seq_len(n_coef), function(j) {
      results[, paste0("MSE_", prefix, j)]
    }),
    sapply(seq_len(n_coef), function(j) {
      results[, paste0("MedBias_", prefix, j)]
    }),
    sapply(seq_len(n_coef), function(j) {
      results[, paste0("MAE_", prefix, j)]
    }),
    sapply(seq_len(n_coef), function(j) {
      results[, paste0("MedAE_", prefix, j)]
    })
  )
  
  colnames(df) <- as.vector(
    sapply(
      metric_names,
      function(m) {
        paste0(m, "_", prefix, seq_len(n_coef))
      }
    )
  )
  
  df$Censoring_Percent <- results[, cp_name]
  
  df
}

metrics_scls     <- extract_metrics(results_scls,     "s", "cp_s")
metrics_quantile <- extract_metrics(results_quantile, "q", "cp_q")
metrics_WME      <- extract_metrics(results_WME,      "W", "cp_h")
metrics_hampel   <- extract_metrics(results_hampel,   "h", "cp_hm")
metrics_tukey    <- extract_metrics(results_tukey,    "T", "cp_t")

idx <- seq_len(nrow(metrics_scls))

cat("Quantile results:\n")
print(round(data.frame(n = sample_size[idx], metrics_quantile[idx, ]), 4))

cat("\nSCLS results:\n")
print(round(data.frame(n = sample_size[idx], metrics_scls[idx, ]), 4))

cat("\nHuber results:\n")
print(round(data.frame(n = sample_size[idx], metrics_WME[idx, ]), 4))

cat("\nHampel results:\n")
print(round(data.frame(n = sample_size[idx], metrics_hampel[idx, ]), 4))

cat("\nTukey results:\n")
print(round(data.frame(n = sample_size[idx], metrics_tukey[idx, ]), 4))

# ---------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------

par(mfrow = c(2, 2))

plot_metric <- function(metric_list, main, ylab) {
  
  cols <- c("red", "blue", "green4", "orange", "purple")
  
  plot(
    sample_size,
    metric_list[[1]],
    type = "o",
    col = cols[1],
    pch = 21,
    xlab = "Sample size",
    ylab = ylab,
    main = main,
    ylim = range(unlist(metric_list))
  )
  
  for (i in seq_along(metric_list)[-1]) {
    lines(
      sample_size,
      metric_list[[i]],
      col = cols[i],
      type = "o",
      pch = 20 + i
    )
  }
  
  abline(h = 0, col = "black", lty = 2)
  
  legend(
    "topright",
    c("Quantile", "SCLS", "Huber", "Hampel", "Tukey"),
    col = cols,
    lty = 1,
    cex = 0.7
  )
}

metric_name <- "MSE"
# Change to: "MeanBias", "MedBias", "MAE", or "MedAE"

for (j in seq_len(n_coef)) {
  
  plot_metric(
    list(
      metrics_quantile[[paste0(metric_name, "_q", j)]],
      metrics_scls[[paste0(metric_name, "_s", j)]],
      metrics_WME[[paste0(metric_name, "_W", j)]],
      metrics_hampel[[paste0(metric_name, "_h", j)]],
      metrics_tukey[[paste0(metric_name, "_T", j)]]
    ),
    main = paste0(metric_name, " of beta_", j - 1),
    ylab = metric_name
  )
}
