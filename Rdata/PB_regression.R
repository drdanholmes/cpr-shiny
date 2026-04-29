#  cp-R: Chemical Pathology R — Passing-Bablok Regression
#  Copyright (C) 2014  Daniel T. Holmes, MD  |  GNU GPL v3
#
#  Point estimates and CIs via the mcr package (CLSI EP09-validated).
#  Confidence band drawn from our own bootstrap envelope — mcr does not
#  expose per-replicate (a, b) pairs needed to construct the pointwise band.

options(menu.graphics = FALSE)

# ── Packages ──────────────────────────────────────────────────────────────────
for (pkg in c("mcr", "compiler", "boot", "ggplot2", "plotly", "htmlwidgets")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(mcr)
library(compiler); library(boot)
library(ggplot2);  library(plotly); library(htmlwidgets)

# ── Arguments ─────────────────────────────────────────────────────────────────
args   <- commandArgs(TRUE)
tmpdir <- eval(parse(text = args))

# ── Helpers ───────────────────────────────────────────────────────────────────
addazero <- function(x) if (nchar(x) == 1) paste0("0", x) else x

col2hex <- function(name) {
  rgb <- col2rgb(name)
  r   <- addazero(as.character(structure(rgb[1], class = "hexmode")))
  g   <- addazero(as.character(structure(rgb[2], class = "hexmode")))
  b   <- addazero(as.character(structure(rgb[3], class = "hexmode")))
  paste0("#", r, g, b)
}

to_bool <- function(x) as.logical(toupper(as.character(x)))

open_device <- function(fmt, path, pp) {
  switch(fmt,
    jpg  = jpeg(path,      width = pp$my_width, height = pp$my_height,
                units = "in", res = pp$my_dpi, pointsize = pp$font_mult),
    png  = png(path,       width = pp$my_width, height = pp$my_height,
               units = "in", res = pp$my_dpi, pointsize = pp$font_mult),
    tiff = tiff(path,      width = pp$my_width, height = pp$my_height,
                units = "in", res = pp$my_dpi, pointsize = pp$font_mult),
    bmp  = bmp(path,       width = pp$my_width, height = pp$my_height,
               units = "in", res = pp$my_dpi, pointsize = pp$font_mult),
    pdf  = cairo_pdf(path, width = pp$my_width, height = pp$my_height,
                     pointsize = pp$font_mult),
    ps   = cairo_ps(path,  width = pp$my_width, height = pp$my_height,
                    pointsize = pp$font_mult)
  )
}

# ── mcr wrapper — point estimates and analytical CIs ─────────────────────────
PB.mcr <- function(x, y, alpha = 0.05) {
  fit  <- mcreg(x, y, method.reg = "PaBa", alpha = alpha,
                mref.name = "x", mtest.name = "y")
  para <- fit@para
  list(
    intercept    = para["Intercept", "EST"],
    slope        = para["Slope",     "EST"],
    CI.intercept = c(para["Intercept", "LCI"], para["Intercept", "UCI"]),
    CI.slope     = c(para["Slope",     "LCI"], para["Slope",     "UCI"])
  )
}

# ── Bootstrap envelope — for the confidence band only ────────────────────────
# mcr does not expose per-replicate (a, b) pairs so we keep our own bootstrap
# here solely to construct the pointwise band across the x range.
PB.reg <- function(data, alpha, indices) {
  d  <- data[indices, ]
  x  <- d[, 1]; y <- d[, 2]
  lx <- length(x)
  k  <- 0L
  S  <- numeric(choose(lx, 2))
  for (i in seq_len(lx - 1))
    for (j in (i + 1):lx)
      if (x[i] != x[j] && !(y[i] == y[j] && x[i] < x[j])) {
        k    <- k + 1L
        S[k] <- (y[i] - y[j]) / (x[i] - x[j])
      }
  if (k == 0L) return(c(NA_real_, NA_real_))
  S  <- sort(S[seq_len(k)])
  N  <- length(S)
  K  <- sum(S < 0) %/% 2L
  b  <- if (N %% 2 == 1) S[(N + 1) / 2 + K]
        else 0.5 * (S[N/2 + K] + S[N/2 + K + 1])
  a  <- median(y - b * x)
  c(a, b)
}
PB.reg.comp <- cmpfun(PB.reg)

PB.boot.band <- function(data, R, n.fit, alpha, xmin, xmax) {
  br   <- boot(data = data, alpha = alpha, statistic = PB.reg.comp, R = R)
  ok   <- complete.cases(br$t)
  av   <- br$t[ok, 1]; bv <- br$t[ok, 2]
  xs   <- seq(xmin - 0.1*abs(xmax - xmin), xmax + 0.1*abs(xmax - xmin),
              length.out = n.fit)
  # Vectorised: outer product avoids a loop over n.fit points
  ymat <- outer(bv, xs) + av
  data.frame(
    x   = xs,
    lwr = apply(ymat, 2, quantile, alpha / 2),
    upr = apply(ymat, 2, quantile, 1 - alpha / 2)
  )
}

# ── Base-R regression plot ────────────────────────────────────────────────────
plot.graph <- function(data, R, n.fit, alpha, pp, reg) {
  a <- reg$intercept; b <- reg$slope
  x <- data[, 1];     y <- data[, 2]

  ci <- if (pp$plot_confidence)
    PB.boot.band(data, R, n.fit, alpha, pp$xmin, pp$xmax)
  else
    NULL

  transp_flag <- pp$confidence_transp > 90
  pp$confidence_transp <- round(pp$confidence_transp * 2.55, 0)
  class(pp$confidence_transp) <- "hexmode"
  cffill <- paste0(col2hex(as.character(pp$confidence_fill_col)),
                   addazero(as.character(pp$confidence_transp)))

  plot(x, y, pch=pp$my_pch, cex=pp$my_cex, lty=pp$my_lty, lwd=pp$my_lwd,
       col=as.character(pp$my_point_col), bg=as.character(pp$my_bg),
       xlim=c(pp$xmin, pp$xmax), ylim=c(pp$ymin, pp$ymax),
       xlab=as.character(pp$my_xlab), ylab=as.character(pp$my_ylab),
       main=as.character(pp$my_main))

  if (!is.null(ci)) {
    for (i in seq_len(nrow(ci) - 1)) {
      vx     <- c(ci$x[i], ci$x[i], ci$x[i+1], ci$x[i+1])
      vy     <- c(ci$lwr[i], ci$upr[i], ci$upr[i+1], ci$lwr[i+1])
      border <- if ((pp$plot_type %in% c("ps","pdf")) && transp_flag) cffill else NA
      polygon(vx, vy, fillOddEven = FALSE, col = cffill, border = border)
    }
    if (pp$plot_conf_outline) {
      lines(ci$x, ci$lwr, lty=2, lwd=pp$my_lwd, col=as.character(pp$conf_col))
      lines(ci$x, ci$upr, lty=2, lwd=pp$my_lwd, col=as.character(pp$conf_col))
    }
  }

  points(x, y, pch=pp$my_pch, cex=pp$my_cex, lwd=pp$my_lwd,
         col=as.character(pp$my_point_col), bg=as.character(pp$my_bg))
  abline(a, b, lty=pp$my_lty, lwd=pp$my_lwd, col=as.character(pp$my_lincol))
  if (pp$plot_identity) abline(0, 1, lwd=pp$my_lwd, lty=2, col="red")

  int_s <- sprintf("%.2f", round(a, 2))
  slp_s <- sprintf("%.2f", round(b, 2))
  eq    <- if (a >= 0) paste0("y=",slp_s,"x+",int_s)
           else         paste0("y=",slp_s,"x-",abs(as.numeric(int_s)))
  r2_eq <- bquote(R^2*"="*.(round(cor.test(y,x)$estimate^2, 4)))
  ypos  <- c(pp$ymin + 0.98*abs(pp$ymax - pp$ymin),
             pp$ymin + 0.92*abs(pp$ymax - pp$ymin),
             pp$ymin + 0.86*abs(pp$ymax - pp$ymin))
  items <- list()
  if (pp$plot_regression) items <- c(items, list(list(eq,                       ypos[length(items)+1])))
  if (pp$plot_rsquared)   items <- c(items, list(list(r2_eq,                    ypos[length(items)+1])))
  if (pp$plot_method)     items <- c(items, list(list("Method: Passing-Bablok", ypos[length(items)+1])))
  for (it in items) text(pp$xmin, it[[2]], it[[1]], adj = c(0, 0))

  invisible(ci)   # return band for re-use in plotly widget
}

# ── Data ──────────────────────────────────────────────────────────────────────
my.data        <- read.csv(file.path(tmpdir, "Rdata", "regression_data.csv"),
                           header = FALSE, sep = "\t")
names(my.data) <- c("x", "y")
my.data$x      <- as.numeric(as.character(my.data$x))
my.data$y      <- as.numeric(as.character(my.data$y))
my.data        <- my.data[complete.cases(my.data), ]
my.data$row    <- as.integer(rownames(my.data))

# ── Parameters ────────────────────────────────────────────────────────────────
pp <- read.csv(file.path(tmpdir, "Rdata", "plot_parameters.csv"),
               header = TRUE, sep = "\t", quote = "\"")
for (col in c("plot_regression","plot_rsquared","plot_method","plot_identity",
              "plot_confidence","plot_conf_outline","plot_difference_abs"))
  pp[[col]] <- to_bool(pp[[col]])

if (is.na(pp$xmin)) pp$xmin <- min(my.data$x)
if (is.na(pp$xmax)) pp$xmax <- max(my.data$x)
if (is.na(pp$ymin)) pp$ymin <- min(my.data$y)
if (is.na(pp$ymax)) pp$ymax <- max(my.data$y)

R <- 500; n.fit <- 50; alpha <- 0.05

# ── Run mcr for validated point estimates and CIs ────────────────────────────
reg <- PB.mcr(my.data$x, my.data$y, alpha = alpha)

# ── Static plots ──────────────────────────────────────────────────────────────
dir.create(file.path(tmpdir, "previews"), showWarnings = FALSE)
dir.create(file.path(tmpdir, "plots"),    showWarnings = FALSE)

jpeg(file.path(tmpdir, "previews", "A.jpg"),
     width=pp$my_width, height=pp$my_height, units="in",
     res=pp$my_dpi, pointsize=pp$font_mult)
ci_band <- plot.graph(my.data[,1:2], R, n.fit, alpha, pp, reg)
dev.off()

for (fmt in c("jpg","png","tiff","bmp","pdf","ps")) {
  tryCatch({
    open_device(fmt, file.path(tmpdir,"plots",paste0("plot.",fmt)), pp)
    plot.graph(my.data[,1:2], R, n.fit, alpha, pp, reg)
    dev.off()
  }, error = function(e) { try(dev.off(), silent=TRUE) })
}

# ── Statistical summary ───────────────────────────────────────────────────────
n       <- nrow(my.data)
int_est <- round(reg$intercept,    3)
slp_est <- round(reg$slope,        3)
ci_int  <- round(reg$CI.intercept, 3)
ci_slp  <- round(reg$CI.slope,     3)
resid   <- my.data$y - (reg$intercept + reg$slope * my.data$x)
rse     <- round(sqrt(sum(resid^2) / (n - 2)), 3)
r2      <- round(cor(my.data$x, my.data$y)^2, 6)

sf <- file.path(tmpdir, "plots", "stats_output.txt")
writeLines(c("Regression Summary", "", "Method: Passing-Bablok",
             paste0("Number of complete cases: ", n, "\n")), sf)
cat(sprintf(
  "Intercept: %s\nCI Intercept: [%s,%s]\n\nSlope: %s\nCI Slope: [%s,%s]\n\nResidual Standard Error: %s on %d degrees of freedom\nR-squared: %s\n",
  int_est, ci_int[1], ci_int[2],
  slp_est, ci_slp[1], ci_slp[2],
  rse, n - 2, r2), file = sf, append = TRUE)

# ── Interactive plotly widget ─────────────────────────────────────────────────
dir.create(file.path(tmpdir, "widgets"), showWarnings = FALSE)

# Reuse band from preview run; recompute only if confidence was off
ci_df <- if (!is.null(ci_band)) ci_band else
  PB.boot.band(my.data[,1:2], R = 200, n.fit = 50,
               alpha = alpha, xmin = pp$xmin, xmax = pp$xmax)

my.data$tip <- paste0("Row: ", my.data$row,
                      "<br>x: ", round(my.data$x, 4),
                      "<br>y: ", round(my.data$y, 4))

a_val  <- reg$intercept; b_val <- reg$slope
eq_str <- paste0("y=", sprintf("%.2f", b_val), "x",
                 ifelse(a_val >= 0, paste0("+", sprintf("%.2f", a_val)),
                                    paste0("-", sprintf("%.2f", abs(a_val)))))
annot_parts <- c()
if (pp$plot_regression) annot_parts <- c(annot_parts, eq_str)
if (pp$plot_rsquared)   annot_parts <- c(annot_parts, paste0("R\u00b2=", round(r2, 4)))
if (pp$plot_method)     annot_parts <- c(annot_parts, "Method: Passing-Bablok")
annot_text <- paste(annot_parts, collapse = "\n")

cf_fill <- as.character(pp$confidence_fill_col)
cf_alph <- pp$confidence_transp / 100
cf_bord <- as.character(pp$conf_col)
ln_col  <- as.character(pp$my_lincol)
pt_col  <- as.character(pp$my_point_col)
pt_fill <- as.character(pp$my_bg)
lwd     <- pp$my_lwd * 0.5

g <- ggplot(my.data, aes(x=x, y=y)) +
  coord_cartesian(xlim=c(pp$xmin,pp$xmax), ylim=c(pp$ymin,pp$ymax)) +
  labs(x=as.character(pp$my_xlab), y=as.character(pp$my_ylab),
       title=as.character(pp$my_main)) +
  theme_bw(base_size=pp$font_mult) + theme(plot.title=element_text(hjust=0.5))

if (pp$plot_confidence && !is.null(ci_df)) {
  g <- g + geom_ribbon(data=ci_df, aes(x=x, ymin=lwr, ymax=upr),
                       inherit.aes=FALSE, fill=cf_fill, alpha=cf_alph, colour=NA)
  if (pp$plot_conf_outline)
    g <- g +
      geom_line(data=ci_df, aes(x=x, y=lwr), inherit.aes=FALSE,
                colour=cf_bord, linetype="dashed", linewidth=lwd) +
      geom_line(data=ci_df, aes(x=x, y=upr), inherit.aes=FALSE,
                colour=cf_bord, linetype="dashed", linewidth=lwd)
}
g <- g +
  geom_abline(intercept=a_val, slope=b_val, colour=ln_col,
              linetype=pp$my_lty, linewidth=lwd)
if (pp$plot_identity)
  g <- g + geom_abline(intercept=0, slope=1, colour="red",
                       linetype="dashed", linewidth=lwd)
g <- g + geom_point(aes(text=tip), shape=pp$my_pch, size=pp$my_cex*2.5,
                    colour=pt_col, fill=pt_fill, stroke=pp$my_lwd*0.4)

p <- ggplotly(g, tooltip="text")
if (nzchar(annot_text))
  p <- p %>% layout(annotations=list(list(
    text=gsub("\n","<br>",annot_text), x=0.02, xref="paper",
    y=0.97, yref="paper", xanchor="left", yanchor="top",
    showarrow=FALSE, bgcolor="rgba(255,255,255,0.85)",
    bordercolor="rgba(0,0,0,0.2)", borderwidth=1, borderpad=5,
    font=list(size=13))))
p <- p %>% layout(hoverlabel=list(bgcolor="white", font=list(size=12)))

saveWidget(p, file=file.path(tmpdir,"widgets","regression.html"),
           selfcontained=TRUE)
