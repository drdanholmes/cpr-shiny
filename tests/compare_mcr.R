#  Check the app's regression functions (R/analysis.R) against mcr::mcreg.
#  Run from the project root:  Rscript tests/compare_mcr.R
#  Exits non-zero if any deterministic check differs by more than `tol`.

suppressPackageStartupMessages({ library(mcr); library(boot) })
source("R/analysis.R")

tol <- 1e-6
set.seed(42)
ds <- list()
td <- read.csv("test_data.csv"); ds$test_data <- td[complete.cases(td), ]
x <- runif(80, 1, 100);  ds$constant_error     <- data.frame(x = x + rnorm(80, 0, 2), y = 1.05 * x + 3 + rnorm(80, 0, 2))
x <- runif(100, 1, 200); ds$proportional_error <- data.frame(x = x * (1 + rnorm(100, 0, .05)), y = (0.92 * x + 1) * (1 + rnorm(100, 0, .05)))
x <- sample(1:30, 60, TRUE); ds$integer_ties   <- data.frame(x = x, y = pmax(x + sample(-2:2, 60, TRUE), 1))
x <- runif(40, 5, 50);   ds$negative_slope     <- data.frame(x = x, y = 60 - 0.8 * x + rnorm(40, 0, 3))

res <- list()
check <- function(ds, what, app, ref)
  res[[length(res) + 1]] <<- data.frame(dataset = ds, check = what,
    max_abs_diff = signif(max(abs(app - ref)), 3), pass = max(abs(app - ref)) < tol)
est <- function(m) unname(m@para[, "EST"])
ci  <- function(m, r) unname(m@para[r, c("LCI", "UCI")])
ls_ci <- function(a, k) {
  se <- summary(a$lm)$coefficients[k, 2]
  c(a$intercept, a$slope)[k] + c(-1, 1) * qt(0.975, nrow(a$data) - 2) * se
}
quiet <- function(e) suppressMessages(suppressWarnings(e))

for (nm in names(ds)) {
  d <- ds[[nm]]; d$row <- seq_len(nrow(d))
  pos <- all(d$x > 0 & d$y > 0)

  a <- fit_ls(d); m <- mcreg(d$x, d$y, method.reg = "LinReg", method.ci = "analytical")
  check(nm, "LS estimates",      c(a$intercept, a$slope), est(m))
  check(nm, "LS intercept CI",   ls_ci(a, 1), ci(m, "Intercept"))
  check(nm, "LS slope CI",       ls_ci(a, 2), ci(m, "Slope"))

  if (pos) {
    a <- fit_ls(d, TRUE); m <- mcreg(d$x, d$y, method.reg = "WLinReg", method.ci = "analytical")
    check(nm, "WLS estimates",   c(a$intercept, a$slope), est(m))
    check(nm, "WLS slope CI",    ls_ci(a, 2), ci(m, "Slope"))
  }

  m <- mcreg(d$x, d$y, method.reg = "PaBa", method.ci = "analytical")
  a <- fit_pb(d, band = FALSE)
  check(nm, "PB estimates",      c(a$intercept, a$slope), est(m))
  check(nm, "PB slope CI",       a$ci_slope, ci(m, "Slope"))
  # mcr's default slope.measure = "radian" works via angles (~1e-6 rounding);
  # "tangent" uses the slopes directly, as pb_coef() does
  m <- mcreg(d$x, d$y, method.reg = "PaBa", method.ci = "analytical", slope.measure = "tangent")
  check(nm, "pb_coef() (band)",  pb_coef(d[, c("x", "y")], seq_len(nrow(d))), est(m))

  # App ratio is Var(y)/Var(x); mcr error.ratio is Var(x)/Var(y)
  for (ratio in c(1, 0.25, 4)) {
    a <- fit_deming(d, ratio, FALSE)
    m <- quiet(mcreg(d$x, d$y, method.reg = "Deming", error.ratio = 1 / ratio, method.ci = "jackknife"))
    check(nm, sprintf("Deming estimates, ratio %g", ratio), c(a$intercept, a$slope), est(m))
    if (pos) {
      a <- fit_deming(d, ratio, TRUE)
      m <- quiet(mcreg(d$x, d$y, method.reg = "WDeming", error.ratio = 1 / ratio, method.ci = "jackknife"))
      check(nm, sprintf("WDeming estimates, ratio %g", ratio), c(a$intercept, a$slope), est(m))
    }
  }
}

out <- do.call(rbind, res)
print(out, row.names = FALSE, right = FALSE)
cat(sprintf("\n%d of %d checks passed (tolerance %g)\n", sum(out$pass), nrow(out), tol))
if (!all(out$pass)) quit(status = 1)
