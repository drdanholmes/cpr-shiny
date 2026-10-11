#  cp-R: Chemical Pathology R — regression and Bland-Altman calculations
#  Copyright (C) 2014  Daniel T. Holmes, MD  |  GNU GPL v3
#
#  Pure functions: data in, result list out. No files, no plotting.
#  `d` is always a data.frame with columns x, y and row (the row number shown
#  in the data table, carried through to plot tooltips).
#  Requires packages: boot, mcr.

# ── Helpers ───────────────────────────────────────────────────────────────────
# Fill any NA axis limit from the data
fill_limits <- function(pp, d) {
  if (is.na(pp$xmin)) pp$xmin <- min(d$x)
  if (is.na(pp$xmax)) pp$xmax <- max(d$x)
  if (is.na(pp$ymin)) pp$ymin <- min(d$y)
  if (is.na(pp$ymax)) pp$ymax <- max(d$y)
  pp
}

# x grid for confidence bands: axis range extended 10% each side
band_grid <- function(xlim, n) {
  span <- abs(xlim[2] - xlim[1])
  seq(xlim[1] - 0.1 * span, xlim[2] + 0.1 * span, length.out = n)
}

# ── Least squares ─────────────────────────────────────────────────────────────
fit_ls <- function(d, weighted = FALSE) {
  n_in <- nrow(d)
  if (weighted) d <- d[d$x != 0, , drop = FALSE]   # 1/x² undefined at zero
  w   <- if (weighted) 1 / d$x^2 else rep(1, nrow(d))
  reg <- lm(y ~ x, data = data.frame(x = d$x, y = d$y, w = w), weights = w)

  list(method       = "LS",
       method_label = if (weighted) "Least Squares, weighted" else "Least Squares",
       weighted     = weighted,
       data         = d,
       n_zero_x     = n_in - nrow(d),
       intercept    = unname(coef(reg)[1]),
       slope        = unname(coef(reg)[2]),
       lm           = reg,
       w            = w)
}

# ── Passing-Bablok ────────────────────────────────────────────────────────────
# Point estimates and CIs via mcr (CLSI EP09-validated). mcr does not expose
# per-replicate (a, b) pairs, so the pointwise confidence band comes from our
# own bootstrap of pb_coef().
pb_coef <- function(data, indices) {
  x <- data[indices, 1]; y <- data[indices, 2]
  n <- length(x)
  # All pairs i < j, vectorised
  i <- rep.int(seq_len(n - 1), (n - 1):1)
  j <- sequence((n - 1):1, from = 2:n)
  keep <- x[i] != x[j] & !(y[i] == y[j] & x[i] < x[j])
  if (!any(keep)) return(c(NA_real_, NA_real_))
  S <- sort((y[i][keep] - y[j][keep]) / (x[i][keep] - x[j][keep]))
  N <- length(S)
  K <- sum(S < 0) %/% 2L
  b <- if (N %% 2 == 1) S[(N + 1) / 2 + K]
       else 0.5 * (S[N/2 + K] + S[N/2 + K + 1])
  c(median(y - b * x), b)
}

fit_pb <- function(d, alpha = 0.05, band = TRUE, R = 500) {
  fit  <- mcreg(d$x, d$y, method.reg = "PaBa", alpha = alpha,
                mref.name = "x", mtest.name = "y")
  para <- fit@para
  boot_coef <- NULL
  if (band) {
    br <- boot(data = d[, c("x", "y")], statistic = pb_coef, R = R)
    boot_coef <- br$t[complete.cases(br$t), , drop = FALSE]
  }
  list(method       = "PB",
       method_label = "Passing-Bablok",
       data         = d,
       intercept    = para["Intercept", "EST"],
       slope        = para["Slope",     "EST"],
       ci_intercept = c(para["Intercept", "LCI"], para["Intercept", "UCI"]),
       ci_slope     = c(para["Slope",     "LCI"], para["Slope",     "UCI"]),
       boot_coef    = boot_coef,
       alpha        = alpha)
}

# ── Deming ────────────────────────────────────────────────────────────────────
# delta = Var(y)/Var(x); data columns 1..3 are x, y, w
deming_coef <- function(data, delta, indices = seq_len(nrow(data))) {
  d      <- data[indices, ]
  x      <- d[, 1]; y <- d[, 2]; w <- d[, 3]
  lambda <- 1 / delta
  xbarw  <- sum(w * x) / sum(w)
  ybarw  <- sum(w * y) / sum(w)
  uw <- sum(w * (x - xbarw)^2)
  pw <- sum(w * (x - xbarw) * (y - ybarw))
  qw <- sum(w * (y - ybarw)^2)
  b  <- ((lambda*qw - uw) + sqrt((uw - lambda*qw)^2 + 4*lambda*pw^2)) / (2*lambda*pw)
  c(ybarw - b * xbarw, b)
}

# Weights (iterated to convergence if weighted) and fitted Xhat, Yhat
deming_weights <- function(x, y, delta, weighted, max_iter = 500) {
  lambda <- 1 / delta
  w      <- rep(1, length(x))
  repeat {
    ab   <- deming_coef(data.frame(x, y, w), delta)
    dist <- y - (ab[1] + ab[2] * x)
    Xhat <- x + lambda * ab[2] * dist / (1 + lambda * ab[2]^2)
    Yhat <- y - dist / (1 + lambda * ab[2]^2)
    if (!weighted) break
    w_old <- w
    w     <- 1 / ((Xhat + Yhat) / 2)^2
    max_iter <- max_iter - 1
    if (abs(max((w_old - w) / w_old)) <= 1e-5 || max_iter == 0) break
  }
  list(w = w, Xhat = Xhat, Yhat = Yhat)
}

fit_deming <- function(d, delta = 1, weighted = FALSE, alpha = 0.05, R = 1000) {
  wt <- deming_weights(d$x, d$y, delta, weighted)
  dd <- data.frame(x = d$x, y = d$y, w = wt$w)
  ab <- deming_coef(dd, delta)

  br <- boot(data = dd, statistic = deming_coef, R = R, delta = delta)
  ok <- complete.cases(br$t)
  br$t <- br$t[ok, , drop = FALSE]
  br$R <- sum(ok)
  a_ci <- boot.ci(br, type = "bca", index = 1)$bca[4:5]
  b_ci <- boot.ci(br, type = "bca", index = 2)$bca[4:5]

  lambda <- 1 / delta
  sgn    <- sign(d$y - wt$Yhat)
  list(method       = "Deming",
       method_label = if (weighted) "Deming, weighted" else "Deming",
       weighted     = weighted,
       data         = d,
       intercept    = ab[1],
       slope        = ab[2],
       ci_intercept = a_ci,
       ci_slope     = b_ci,
       boot_coef    = br$t,
       alpha        = alpha,
       resid        = sgn * sqrt(wt$w * (d$x - wt$Xhat)^2 + wt$w * lambda * (d$y - wt$Yhat)^2),
       resid_raw    = sgn * sqrt((d$x - wt$Xhat)^2 + lambda * (d$y - wt$Yhat)^2))
}

# ── Confidence band for any fit ───────────────────────────────────────────────
# Returns data.frame(x, lwr, upr): analytical for least squares, bootstrap
# percentile envelope otherwise.
fit_band <- function(fit, xlim) {
  if (fit$method == "LS") {
    xs  <- band_grid(xlim, 200)
    prd <- predict(fit$lm, newdata = data.frame(x = xs),
                   interval = "confidence", level = 0.95)
    return(data.frame(x = xs, lwr = prd[, "lwr"], upr = prd[, "upr"]))
  }
  if (is.null(fit$boot_coef)) return(NULL)
  xs   <- band_grid(xlim, 50)
  ymat <- outer(fit$boot_coef[, 2], xs) + fit$boot_coef[, 1]
  data.frame(x   = xs,
             lwr = apply(ymat, 2, quantile, fit$alpha / 2,     na.rm = TRUE),
             upr = apply(ymat, 2, quantile, 1 - fit$alpha / 2, na.rm = TRUE))
}

# ── Bland-Altman ──────────────────────────────────────────────────────────────
fit_ba <- function(d, abs_diff = TRUE) {
  # For percent difference, x = y = 0 causes division by zero
  if (!abs_diff) d <- d[!(d$x == d$y & d$x == 0), , drop = FALSE]
  diff <- if (abs_diff) d$y - d$x else (d$y - d$x) / (d$y + d$x) * 200
  list(data      = d,
       abs_diff  = abs_diff,
       avg       = (d$x + d$y) / 2,
       diff      = diff,
       mean_diff = mean(diff),
       sd_diff   = sd(diff))
}

# ── Statistics text ───────────────────────────────────────────────────────────
stats_text <- function(fit) {
  d  <- fit$data
  n  <- nrow(d)
  r2 <- round(cor(d$x, d$y)^2, 6)

  if (fit$method == "LS") {
    reg <- fit$lm
    df2 <- n - 2
    se  <- summary(reg)$coefficients[, 2]
    int_est <- round(fit$intercept, 3)
    slp_est <- round(fit$slope,     3)
    ci_int  <- round(int_est + c(-1, 1) * qt(0.975, df2) * se[1], 3)
    ci_slp  <- round(slp_est + c(-1, 1) * qt(0.975, df2) * se[2], 3)
    rse_raw <- round(sqrt(sum(reg$residuals^2) / df2), 3)
    rse_wtd <- round(sqrt(sum((sqrt(fit$w) * reg$residuals)^2) / df2), 3)
    out <- c("Regression Summary\n\n",
             "Method: ", if (fit$weighted) "LSR, weighted" else "LSR", "\n",
             "Number of complete cases: ", n, "\n\n")
    if (fit$n_zero_x > 0)
      out <- c(out, sprintf("Warning: %d record(s) with x=0 excluded for 1/x² weighting\n\n",
                            fit$n_zero_x))
    out <- c(out, sprintf(
      "Intercept: %s\nCI Intercept: [%s,%s]\n\nSlope: %s\nCI Slope: [%s,%s]\n\nResidual Standard Error: %s on %d degrees of freedom\n",
      int_est, ci_int[1], ci_int[2], slp_est, ci_slp[1], ci_slp[2], rse_raw, df2))
    if (fit$weighted)
      out <- c(out, sprintf("(1/x² Weighted RSE: %s)\n", rse_wtd))
    out <- c(out, sprintf("R-squared: %s\n", round(summary(reg)$r.squared, 6)))
    return(paste(out, collapse = ""))
  }

  resid <- if (fit$method == "Deming") fit$resid_raw
           else d$y - (fit$intercept + fit$slope * d$x)
  rse   <- round(sqrt(sum(resid^2) / (n - 2)), 3)
  out <- c("Regression Summary\n\n",
           "Method: ", fit$method_label, "\n",
           "Number of complete cases: ", n, "\n\n",
           sprintf("Intercept: %s\nCI Intercept: [%s,%s]\n\nSlope: %s\nCI Slope: [%s,%s]\n\nResidual Standard Error: %s on %d degrees of freedom\n",
                   round(fit$intercept, 3),
                   round(fit$ci_intercept[1], 3), round(fit$ci_intercept[2], 3),
                   round(fit$slope, 3),
                   round(fit$ci_slope[1], 3), round(fit$ci_slope[2], 3),
                   rse, n - 2))
  if (isTRUE(fit$weighted))
    out <- c(out, sprintf("(Weighted RSE: %s)\n\n",
                          round(sqrt(sum(fit$resid^2) / (n - 2)), 3)))
  out <- c(out, sprintf("R-squared: %s\n", r2))
  paste(out, collapse = "")
}

ba_stats_text <- function(ba) {
  qd  <- signif(quantile(ba$diff, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)), 4)
  pct <- if (ba$abs_diff) "" else "%"
  sprintf("\nMean Difference: %s%s\nSD Difference: %s%s\n\nQuantiles of Difference%s: [2.5,25,50,75,97.5]\n[%s,%s,%s,%s,%s]\n",
          signif(ba$mean_diff, 4), pct, signif(ba$sd_diff, 4), pct,
          if (ba$abs_diff) "" else "(%)",
          qd[1], qd[2], qd[3], qd[4], qd[5])
}
