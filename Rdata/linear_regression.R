#  cp-R: Chemical Pathology R — Least Squares Regression
#  Copyright (C) 2014  Daniel T. Holmes, MD  |  GNU GPL v3

options(menu.graphics = FALSE)

# ── Packages ──────────────────────────────────────────────────────────────────
for (pkg in c("ggplot2", "plotly", "htmlwidgets")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(ggplot2); library(plotly); library(htmlwidgets)

# ── Arguments ─────────────────────────────────────────────────────────────────
args   <- commandArgs(TRUE)
tmpdir <- eval(parse(text = args))

# ── Helpers ───────────────────────────────────────────────────────────────────
# Hex colour with alpha (used by base-R polygon confidence band)
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

# ── Base-R regression plot ────────────────────────────────────────────────────
plot.graph <- function(x, y, w, pp) {
  regression <- lm(y ~ x, weights = w)

  # Colour helpers
  transp_flag <- pp$confidence_transp > 90
  pp$confidence_transp <- round(pp$confidence_transp * 2.55, 0)
  class(pp$confidence_transp) <- "hexmode"
  cftrans <- addazero(as.character(pp$confidence_transp))
  cffill  <- paste0(col2hex(as.character(pp$confidence_fill_col)), cftrans)

  # Main scatter
  plot(x, y,
       pch = pp$my_pch, cex = pp$my_cex, lty = pp$my_lty, lwd = pp$my_lwd,
       col = as.character(pp$my_point_col), bg = as.character(pp$my_bg),
       xlim = c(pp$xmin, pp$xmax), ylim = c(pp$ymin, pp$ymax),
       xlab = as.character(pp$my_xlab), ylab = as.character(pp$my_ylab),
       main = as.character(pp$my_main))

  # Confidence band
  if (pp$plot_confidence) {
    xs  <- seq(pp$xmin - 0.1*abs(pp$xmax - pp$xmin),
               pp$xmax + 0.1*abs(pp$xmax - pp$xmin), length.out = 50)
    prd <- predict(regression, newdata = data.frame(x = xs),
                   interval = "confidence", level = 0.95)
    for (i in seq_len(length(xs) - 1)) {
      vx <- c(xs[i], xs[i], xs[i+1], xs[i+1])
      vy <- c(prd[i,2], prd[i,3], prd[i+1,3], prd[i+1,2])
      border <- if ((pp$plot_type %in% c("ps","pdf")) && transp_flag) cffill else NA
      polygon(vx, vy, fillOddEven = FALSE, col = cffill, border = border)
    }
    if (pp$plot_conf_outline) {
      lines(xs, prd[,2], lty = 2, lwd = pp$my_lwd, col = as.character(pp$conf_col))
      lines(xs, prd[,3], lty = 2, lwd = pp$my_lwd, col = as.character(pp$conf_col))
    }
  }

  # Replot points (confidence band may have obscured them)
  points(x, y,
         pch = pp$my_pch, cex = pp$my_cex, lwd = pp$my_lwd,
         col = as.character(pp$my_point_col), bg = as.character(pp$my_bg))

  # Regression line
  abline(regression$coefficients[1], regression$coefficients[2],
         lty = pp$my_lty, lwd = pp$my_lwd, col = as.character(pp$my_lincol))

  # Identity line
  if (pp$plot_identity) abline(0, 1, lwd = pp$my_lwd, lty = 2, col = "red")

  # Annotation text (equation, R², method)
  int_v <- regression$coefficients[1]
  slp_v <- regression$coefficients[2]
  int_s <- sprintf("%.2f", round(int_v, 2))
  slp_s <- sprintf("%.2f", round(slp_v, 2))
  eq    <- if (int_v >= 0) paste0("y=", slp_s, "x+", int_s)
           else             paste0("y=", slp_s, "x-", abs(as.numeric(int_s)))
  r2_eq <- bquote(R^2*"="*.(round(cor.test(y,x)$estimate^2, 4)))
  meth  <- if (pp$weighting) "Method: Least Squares, weighted" else "Method: Least Squares"

  ypos <- c(pp$ymin + 0.98*abs(pp$ymax - pp$ymin),
            pp$ymin + 0.92*abs(pp$ymax - pp$ymin),
            pp$ymin + 0.86*abs(pp$ymax - pp$ymin))

  items <- list()
  if (pp$plot_regression) items <- c(items, list(list(eq,   ypos[length(items)+1])))
  if (pp$plot_rsquared)   items <- c(items, list(list(r2_eq, ypos[length(items)+1])))
  if (pp$plot_method)     items <- c(items, list(list(meth,  ypos[length(items)+1])))
  for (it in items) text(pp$xmin, it[[2]], it[[1]], adj = c(0, 0))

  invisible(regression)
}

# ── Data ──────────────────────────────────────────────────────────────────────
my.data      <- read.csv(file.path(tmpdir, "Rdata", "regression_data.csv"),
                         header = FALSE, sep = "\t")
names(my.data) <- c("x", "y")
my.data$x    <- as.numeric(as.character(my.data$x))
my.data$y    <- as.numeric(as.character(my.data$y))
my.data      <- my.data[complete.cases(my.data), ]
my.data$row  <- as.integer(rownames(my.data))

# ── Parameters ────────────────────────────────────────────────────────────────
pp <- read.csv(file.path(tmpdir, "Rdata", "plot_parameters.csv"),
               header = TRUE, sep = "\t", quote = "\"")
for (col in c("plot_regression","plot_rsquared","plot_method","plot_identity",
              "plot_confidence","plot_conf_outline","plot_difference_abs","weighting"))
  pp[[col]] <- to_bool(pp[[col]])

# Remove x=0 rows if weighting (1/x² undefined at zero)
original_n <- nrow(my.data)
if (pp$weighting) my.data <- subset(my.data, x != 0)
records_removed <- nrow(my.data) != original_n

# Axis limits
if (is.na(pp$xmin)) pp$xmin <- min(my.data$x)
if (is.na(pp$xmax)) pp$xmax <- max(my.data$x)
if (is.na(pp$ymin)) pp$ymin <- min(my.data$y)
if (is.na(pp$ymax)) pp$ymax <- max(my.data$y)

w <- if (pp$weighting) 1 / my.data$x^2 else rep(1, nrow(my.data))

# ── Static plots — all formats ────────────────────────────────────────────────
dir.create(file.path(tmpdir, "previews"), showWarnings = FALSE)
dir.create(file.path(tmpdir, "plots"),    showWarnings = FALSE)

jpeg(file.path(tmpdir, "previews", "A.jpg"),
     width = pp$my_width, height = pp$my_height,
     units = "in", res = pp$my_dpi, pointsize = pp$font_mult)
reg <- plot.graph(my.data$x, my.data$y, w, pp)
dev.off()

for (fmt in c("jpg","png","tiff","bmp","pdf","ps")) {
  path <- file.path(tmpdir, "plots", paste0("plot.", fmt))
  tryCatch({
    open_device(fmt, path, pp)
    plot.graph(my.data$x, my.data$y, w, pp)
    dev.off()
  }, error = function(e) { try(dev.off(), silent = TRUE) })
}

resid <- sqrt(w) * reg$resid

# ── Statistical summary ───────────────────────────────────────────────────────
n          <- nrow(my.data)
df2        <- n - 2
int_est    <- round(coef(reg)[1], 3)
slp_est    <- round(coef(reg)[2], 3)
se_int     <- summary(reg)$coefficients[1, 2]
se_slp     <- summary(reg)$coefficients[2, 2]
ci_int     <- round(int_est + c(-1,1) * qt(0.975, df2) * se_int, 3)
ci_slp     <- round(slp_est + c(-1,1) * qt(0.975, df2) * se_slp, 3)
rse_raw    <- round(sqrt(sum(reg$residuals^2) / df2), 3)
rse_wtd    <- round(sqrt(sum(resid^2) / df2), 3)
r2         <- round(summary(reg)$r.squared, 6)

sf <- file.path(tmpdir, "plots", "stats_output.txt")
meth_str <- if (pp$weighting) "LSR, weighted" else "LSR"
writeLines(c(
  "Regression Summary",
  "",
  paste0("Method: ", meth_str),
  paste0("Number of complete cases: ", n, "\n")
), sf)

if (records_removed)
  cat(sprintf("Warning: %d record(s) with x=0 excluded for 1/x\u00b2 weighting\n\n",
              original_n - n), file = sf, append = TRUE)

cat(sprintf("Intercept: %s\nCI Intercept: [%s,%s]\n\nSlope: %s\nCI Slope: [%s,%s]\n\nResidual Standard Error: %s on %d degrees of freedom\n",
            int_est, ci_int[1], ci_int[2],
            slp_est, ci_slp[1], ci_slp[2],
            rse_raw, df2), file = sf, append = TRUE)

if (pp$weighting)
  cat(sprintf("(1/x\u00b2 Weighted RSE: %s)\n", rse_wtd), file = sf, append = TRUE)

cat(sprintf("R-squared: %s\n", r2), file = sf, append = TRUE)

# ── Interactive plotly widget ─────────────────────────────────────────────────
dir.create(file.path(tmpdir, "widgets"), showWarnings = FALSE)

# Tooltip and annotation
my.data$tip <- paste0("Row: ", my.data$row,
                      "<br>x: ", round(my.data$x, 4),
                      "<br>y: ", round(my.data$y, 4))

int_v <- coef(reg)[1]; slp_v <- coef(reg)[2]
eq_str <- paste0("y=", sprintf("%.2f", slp_v), "x",
                 ifelse(int_v >= 0, paste0("+", sprintf("%.2f", int_v)),
                                    paste0("-", sprintf("%.2f", abs(int_v)))))
annot_parts <- c()
if (pp$plot_regression) annot_parts <- c(annot_parts, eq_str)
if (pp$plot_rsquared)   annot_parts <- c(annot_parts,
                                         paste0("R\u00b2=", round(cor(my.data$x,my.data$y)^2, 4)))
if (pp$plot_method)     annot_parts <- c(annot_parts,
                                         paste0("Method: ", if(pp$weighting) "Least Squares, weighted" else "Least Squares"))
annot_text <- paste(annot_parts, collapse = "\n")

cf_fill <- as.character(pp$confidence_fill_col)
cf_alph <- pp$confidence_transp / 100
cf_bord <- as.character(pp$conf_col)
ln_col  <- as.character(pp$my_lincol)
pt_col  <- as.character(pp$my_point_col)
pt_fill <- as.character(pp$my_bg)
lwd     <- pp$my_lwd * 0.5

g <- ggplot(my.data, aes(x = x, y = y)) +
  coord_cartesian(xlim = c(pp$xmin, pp$xmax), ylim = c(pp$ymin, pp$ymax)) +
  labs(x = as.character(pp$my_xlab), y = as.character(pp$my_ylab),
       title = as.character(pp$my_main)) +
  theme_bw(base_size = pp$font_mult) +
  theme(plot.title = element_text(hjust = 0.5))

if (pp$plot_confidence) {
  xs  <- seq(pp$xmin - 0.1*abs(pp$xmax - pp$xmin),
             pp$xmax + 0.1*abs(pp$xmax - pp$xmin), length.out = 200)
  prd <- predict(reg, newdata = data.frame(x = xs),
                 interval = "confidence", level = 0.95)
  cf  <- data.frame(x = xs, lwr = prd[,"lwr"], upr = prd[,"upr"])
  g   <- g + geom_ribbon(data = cf, aes(x=x, ymin=lwr, ymax=upr),
                         inherit.aes = FALSE, fill = cf_fill, alpha = cf_alph, colour = NA)
  if (pp$plot_conf_outline)
    g <- g +
      geom_line(data=cf, aes(x=x, y=lwr), inherit.aes=FALSE,
                colour=cf_bord, linetype="dashed", linewidth=lwd) +
      geom_line(data=cf, aes(x=x, y=upr), inherit.aes=FALSE,
                colour=cf_bord, linetype="dashed", linewidth=lwd)
}

g <- g +
  geom_abline(intercept=int_v, slope=slp_v,
              colour=ln_col, linetype=pp$my_lty, linewidth=lwd)

if (pp$plot_identity)
  g <- g + geom_abline(intercept=0, slope=1, colour="red",
                       linetype="dashed", linewidth=lwd)

g <- g + geom_point(aes(text=tip), shape=pp$my_pch, size=pp$my_cex*2.5,
                    colour=pt_col, fill=pt_fill, stroke=pp$my_lwd*0.4)

p <- ggplotly(g, tooltip = "text")
if (nzchar(annot_text))
  p <- p %>% layout(annotations = list(list(
    text=gsub("\n","<br>",annot_text), x=0.02, xref="paper",
    y=0.97, yref="paper", xanchor="left", yanchor="top",
    showarrow=FALSE, bgcolor="rgba(255,255,255,0.85)",
    bordercolor="rgba(0,0,0,0.2)", borderwidth=1, borderpad=5,
    font=list(size=13))))
p <- p %>% layout(hoverlabel = list(bgcolor="white", font=list(size=12)))

saveWidget(p, file = file.path(tmpdir, "widgets", "regression.html"),
           selfcontained = TRUE)
