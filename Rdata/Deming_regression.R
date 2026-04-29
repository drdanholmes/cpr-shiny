#  cp-R: Chemical Pathology R — Deming Regression
#  Copyright (C) 2014  Daniel T. Holmes, MD  |  GNU GPL v3
#  Algorithm: Linnet, Statistics in Medicine, Vol 9, 1463-73, 1990

options(menu.graphics = FALSE)

# ── Packages ──────────────────────────────────────────────────────────────────
for (pkg in c("boot", "ggplot2", "plotly", "htmlwidgets")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(boot); library(ggplot2); library(plotly); library(htmlwidgets)

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
    jpg  = jpeg(path,      width=pp$my_width, height=pp$my_height,
                units="in", res=pp$my_dpi, pointsize=pp$font_mult),
    png  = png(path,       width=pp$my_width, height=pp$my_height,
               units="in", res=pp$my_dpi, pointsize=pp$font_mult),
    tiff = tiff(path,      width=pp$my_width, height=pp$my_height,
                units="in", res=pp$my_dpi, pointsize=pp$font_mult),
    bmp  = bmp(path,       width=pp$my_width, height=pp$my_height,
               units="in", res=pp$my_dpi, pointsize=pp$font_mult),
    pdf  = cairo_pdf(path, width=pp$my_width, height=pp$my_height,
                     pointsize=pp$font_mult),
    ps   = cairo_ps(path,  width=pp$my_width, height=pp$my_height,
                    pointsize=pp$font_mult)
  )
}

# ── Deming regression (Linnet 1990) ──────────────────────────────────────────
Deming.reg <- function(data, delta, alpha, indices = seq_len(nrow(data))) {
  d  <- data[indices, ]
  x  <- d[,1]; y <- d[,2]; w <- d[,3]
  lambda <- 1 / delta
  n      <- length(x)
  xbarw  <- as.numeric(w %*% x / sum(w))
  ybarw  <- as.numeric(w %*% y / sum(w))
  uw <- sum(w * (x - xbarw)^2)
  pw <- sum(w * (x - xbarw) * (y - ybarw))
  qw <- sum(w * (y - ybarw)^2)
  b  <- ((lambda*qw - uw) + sqrt((uw - lambda*qw)^2 + 4*lambda*pw^2)) / (2*lambda*pw)
  a  <- ybarw - b * xbarw
  c(a, b)
}

weight.calculator <- function(data, delta, weighting) {
  x <- data$x; y <- data$y
  w <- rep(1, length(x))
  lambda <- 1 / delta
  n      <- length(x)
  if (weighting) {
    max.ratio <- 1
    while (max.ratio > 1e-5) {
      w.old <- w
      dc    <- Deming.reg(data.frame(x,y,w=w), delta=delta)
      dist  <- y - (dc[1] + dc[2]*x)
      Xhat  <- x + lambda*dc[2]*dist / (1 + lambda*dc[2]^2)
      Yhat  <- y - dist / (1 + lambda*dc[2]^2)
      w     <- 1 / ((Xhat + Yhat)/2)^2
      max.ratio <- abs(max((w.old - w) / w.old))
    }
  } else {
    dc   <- Deming.reg(data.frame(x,y,w=w), delta=delta)
    dist <- y - (dc[1] + dc[2]*x)
    Xhat <- x + lambda*dc[2]*dist / (1 + lambda*dc[2]^2)
    Yhat <- y - dist / (1 + lambda*dc[2]^2)
  }
  data.frame(w=w, Xhat=Xhat, Yhat=Yhat)
}

Deming.boot <- function(data, delta, R, n.fit, alpha, xmin, xmax) {
  br   <- boot(data=data, delta=delta, alpha=alpha, statistic=Deming.reg, R=R)
  ok   <- complete.cases(br$t)
  br$t <- br$t[ok, , drop=FALSE]
  br$R <- nrow(br$t)
  a.ci <- boot.ci(br, type="bca", index=1)
  b.ci <- boot.ci(br, type="bca", index=2)
  av   <- br$t[,1]; bv <- br$t[,2]
  xs   <- seq(xmin - 0.1*abs(xmax-xmin), xmax + 0.1*abs(xmax-xmin), length.out=n.fit)
  ci   <- matrix(NA_real_, nrow=n.fit, ncol=3)
  for (i in seq_len(n.fit)) {
    yp     <- av + bv * xs[i]
    ci[i,] <- c(xs[i], quantile(yp, alpha/2), quantile(yp, 1 - alpha/2))
  }
  list(reg.CI.data=ci, a.ci=a.ci, b.ci=b.ci)
}

# ── Base-R regression plot ────────────────────────────────────────────────────
plot.graph <- function(data, R, n.fit, alpha, pp) {
  dc     <- Deming.reg(data, delta=pp$delta)
  a      <- dc[1]; b <- dc[2]
  x      <- data[,1]; y <- data[,2]; w <- data[,3]
  Xhat   <- data[,4]; Yhat <- data[,5]
  lambda <- 1 / pp$delta

  boot.r <- Deming.boot(data, delta=pp$delta, R=R, n.fit=n.fit,
                        alpha=alpha, xmin=pp$xmin, xmax=pp$xmax)
  ci     <- boot.r$reg.CI.data

  resid     <- sign(y - Yhat) * sqrt(w*(x-Xhat)^2 + w*lambda*(y-Yhat)^2)
  resid.raw <- sign(y - Yhat) * sqrt((x-Xhat)^2   + lambda*(y-Yhat)^2)

  reg.list <- list(intercept=a, slope=b,
                   CI.intercept=boot.r$a.ci, CI.slope=boot.r$b.ci,
                   resid=resid, resid.raw=resid.raw,
                   fitted=data.frame(Xhat=Xhat, Yhat=Yhat))

  transp_flag <- pp$confidence_transp > 90
  pp$confidence_transp <- round(pp$confidence_transp * 2.55, 0)
  class(pp$confidence_transp) <- "hexmode"
  cffill <- paste0(col2hex(as.character(pp$confidence_fill_col)),
                   addazero(as.character(pp$confidence_transp)))

  plot(x, y, pch=pp$my_pch, cex=pp$my_cex, lty=pp$my_lty, lwd=pp$my_lwd,
       col=as.character(pp$my_point_col), bg=as.character(pp$my_bg),
       xlim=c(pp$xmin,pp$xmax), ylim=c(pp$ymin,pp$ymax),
       xlab=as.character(pp$my_xlab), ylab=as.character(pp$my_ylab),
       main=as.character(pp$my_main))

  if (pp$plot_confidence) {
    for (i in seq_len(n.fit - 1)) {
      vx     <- c(ci[i,1], ci[i,1], ci[i+1,1], ci[i+1,1])
      vy     <- c(ci[i,2], ci[i,3], ci[i+1,3], ci[i+1,2])
      border <- if ((pp$plot_type %in% c("ps","pdf")) && transp_flag) cffill else NA
      polygon(vx, vy, fillOddEven=FALSE, col=cffill, border=border)
    }
    if (pp$plot_conf_outline) {
      lines(ci[,1], ci[,2], lty=2, lwd=pp$my_lwd, col=as.character(pp$conf_col))
      lines(ci[,1], ci[,3], lty=2, lwd=pp$my_lwd, col=as.character(pp$conf_col))
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
  meth  <- if (pp$weighting) "Method: Deming, weighted" else "Method: Deming"
  ypos  <- c(pp$ymin + 0.98*abs(pp$ymax-pp$ymin),
             pp$ymin + 0.92*abs(pp$ymax-pp$ymin),
             pp$ymin + 0.86*abs(pp$ymax-pp$ymin))
  items <- list()
  if (pp$plot_regression) items <- c(items, list(list(eq,    ypos[length(items)+1])))
  if (pp$plot_rsquared)   items <- c(items, list(list(r2_eq, ypos[length(items)+1])))
  if (pp$plot_method)     items <- c(items, list(list(meth,  ypos[length(items)+1])))
  for (it in items) text(pp$xmin, it[[2]], it[[1]], adj=c(0,0))

  invisible(reg.list)
}

# ── Data ──────────────────────────────────────────────────────────────────────
my.data       <- read.csv(file.path(tmpdir,"Rdata","regression_data.csv"),
                          header=FALSE, sep="\t")
names(my.data) <- c("x","y")
my.data$x     <- as.numeric(as.character(my.data$x))
my.data$y     <- as.numeric(as.character(my.data$y))
my.data       <- my.data[complete.cases(my.data),]
my.data$row   <- as.integer(rownames(my.data))

# ── Parameters ────────────────────────────────────────────────────────────────
pp <- read.csv(file.path(tmpdir,"Rdata","plot_parameters.csv"),
               header=TRUE, sep="\t", quote="\"")
for (col in c("plot_regression","plot_rsquared","plot_method","plot_identity",
              "plot_confidence","plot_conf_outline","plot_difference_abs","weighting"))
  pp[[col]] <- to_bool(pp[[col]])

if (is.na(pp$xmin)) pp$xmin <- min(my.data$x)
if (is.na(pp$xmax)) pp$xmax <- max(my.data$x)
if (is.na(pp$ymin)) pp$ymin <- min(my.data$y)
if (is.na(pp$ymax)) pp$ymax <- max(my.data$y)

R <- 1000; n.fit <- 50; alpha <- 0.05

# Compute weights and fitted values
wt      <- weight.calculator(data=my.data, delta=pp$delta, weighting=pp$weighting)
my.data <- data.frame(my.data, wt)

# ── Static plots ──────────────────────────────────────────────────────────────
dir.create(file.path(tmpdir,"previews"), showWarnings=FALSE)
dir.create(file.path(tmpdir,"plots"),    showWarnings=FALSE)

jpeg(file.path(tmpdir,"previews","A.jpg"),
     width=pp$my_width, height=pp$my_height, units="in",
     res=pp$my_dpi, pointsize=pp$font_mult)
reg <- plot.graph(my.data[,1:5], R, n.fit, alpha, pp)
dev.off()

for (fmt in c("jpg","png","tiff","bmp","pdf","ps")) {
  tryCatch({
    open_device(fmt, file.path(tmpdir,"plots",paste0("plot.",fmt)), pp)
    plot.graph(my.data[,1:5], R, n.fit, alpha, pp)
    dev.off()
  }, error = function(e) { try(dev.off(), silent=TRUE) })
}

# ── Statistical summary ───────────────────────────────────────────────────────
n         <- nrow(my.data)
df2       <- n - 2
int_est   <- round(reg$intercept, 3)
slp_est   <- round(reg$slope,     3)
ci_int    <- round(c(reg$CI.intercept$bca[4], reg$CI.intercept$bca[5]), 3)
ci_slp    <- round(c(reg$CI.slope$bca[4],     reg$CI.slope$bca[5]),     3)
rse_raw   <- round(sqrt(sum(reg$resid.raw^2) / df2), 3)
rse_wtd   <- round(sqrt(sum(reg$resid^2)     / df2), 3)
r2        <- round(cor(my.data$x, my.data$y)^2, 6)

sf       <- file.path(tmpdir,"plots","stats_output.txt")
meth_str <- if (pp$weighting) "Deming, weighted" else "Deming"
writeLines(c("Regression Summary","",paste0("Method: ",meth_str),
             paste0("Number of complete cases: ", n, "\n")), sf)
cat(sprintf("Intercept: %s\nCI Intercept (BCa): [%s,%s]\n\nSlope: %s\nCI Slope (BCa): [%s,%s]\n\nResidual Standard Error: %s on %d degrees of freedom\n",
            int_est, ci_int[1], ci_int[2],
            slp_est, ci_slp[1], ci_slp[2],
            rse_raw, df2), file=sf, append=TRUE)
if (pp$weighting)
  cat(sprintf("(Weighted RSE: %s)\n", rse_wtd), file=sf, append=TRUE)
cat(sprintf("R-squared: %s\n", r2), file=sf, append=TRUE)

# ── Interactive plotly widget ─────────────────────────────────────────────────
dir.create(file.path(tmpdir,"widgets"), showWarnings=FALSE)

my.data$tip <- paste0("Row: ", my.data$row,
                      "<br>x: ", round(my.data$x, 4),
                      "<br>y: ", round(my.data$y, 4))

a_val  <- reg$intercept; b_val <- reg$slope
eq_str <- paste0("y=",sprintf("%.2f",b_val),"x",
                 ifelse(a_val>=0, paste0("+",sprintf("%.2f",a_val)),
                                  paste0("-",sprintf("%.2f",abs(a_val)))))
annot_parts <- c()
if (pp$plot_regression) annot_parts <- c(annot_parts, eq_str)
if (pp$plot_rsquared)   annot_parts <- c(annot_parts,
                                         paste0("R\u00b2=",round(r2,4)))
if (pp$plot_method)     annot_parts <- c(annot_parts,
                                         if(pp$weighting) "Method: Deming, weighted" else "Method: Deming")
annot_text <- paste(annot_parts, collapse="\n")

# Rebuild CI data frame with guaranteed numeric columns
boot2  <- Deming.boot(my.data[,1:3], delta=pp$delta, R=200, n.fit=50,
                      alpha=alpha, xmin=pp$xmin, xmax=pp$xmax)
ci_df  <- as.data.frame(boot2$reg.CI.data)
names(ci_df) <- c("x","lwr","upr")
ci_df$x <- as.numeric(ci_df$x); ci_df$lwr <- as.numeric(ci_df$lwr)
ci_df$upr <- as.numeric(ci_df$upr)

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

if (pp$plot_confidence) {
  g <- g + geom_ribbon(data=ci_df, aes(x=x,ymin=lwr,ymax=upr),
                       inherit.aes=FALSE, fill=cf_fill, alpha=cf_alph, colour=NA)
  if (pp$plot_conf_outline)
    g <- g +
      geom_line(data=ci_df,aes(x=x,y=lwr),inherit.aes=FALSE,
                colour=cf_bord,linetype="dashed",linewidth=lwd) +
      geom_line(data=ci_df,aes(x=x,y=upr),inherit.aes=FALSE,
                colour=cf_bord,linetype="dashed",linewidth=lwd)
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
