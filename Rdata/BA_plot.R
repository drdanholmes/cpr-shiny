#  cp-R: Chemical Pathology R — Bland-Altman Difference Plot
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
addazero <- function(x) if (nchar(x) == 1) paste0("0", x) else x

col2hex <- function(name) {
  rgb <- col2rgb(name)
  r   <- addazero(as.character(structure(rgb[1], class = "hexmode")))
  g   <- addazero(as.character(structure(rgb[2], class = "hexmode")))
  b   <- addazero(as.character(structure(rgb[3], class = "hexmode")))
  paste0("#", r, g, b)
}

to_bool <- function(x) as.logical(toupper(as.character(x)))

open_device <- function(fmt, path, my_width, my_height, my_dpi, my_point) {
  switch(fmt,
    jpg  = jpeg(path,      width=my_width, height=my_height,
                units="in", res=my_dpi, pointsize=my_point),
    png  = png(path,       width=my_width, height=my_height,
               units="in", res=my_dpi, pointsize=my_point),
    tiff = tiff(path,      width=my_width, height=my_height,
                units="in", res=my_dpi, pointsize=my_point),
    bmp  = bmp(path,       width=my_width, height=my_height,
               units="in", res=my_dpi, pointsize=my_point),
    pdf  = cairo_pdf(path, width=my_width, height=my_height,
                     pointsize=my_point),
    ps   = cairo_ps(path,  width=my_width, height=my_height,
                    pointsize=my_point)
  )
}

# ── Data ──────────────────────────────────────────────────────────────────────
my.data       <- read.csv(file.path(tmpdir,"Rdata","regression_data.csv"),
                          header=FALSE, sep="\t")
names(my.data) <- c("x","y")
my.data$x     <- as.numeric(as.character(my.data$x))
my.data$y     <- as.numeric(as.character(my.data$y))
my.data$row   <- seq_len(nrow(my.data))   # preserve original row numbers before filtering
my.data       <- my.data[complete.cases(my.data[,c("x","y")]),]

# ── Parameters ────────────────────────────────────────────────────────────────
pp <- read.csv(file.path(tmpdir,"Rdata","plot_parameters.csv"),
               header=TRUE, sep="\t", quote="\"")
for (col in c("plot_identity","plot_confidence","plot_conf_outline","plot_difference_abs"))
  pp[[col]] <- to_bool(pp[[col]])

my_width  <- pp$my_width;  my_height <- pp$my_height
my_dpi    <- pp$my_dpi;    my_point  <- pp$font_mult
my_pch    <- pp$my_pch;    my_cex    <- pp$my_cex
my_lty    <- pp$my_lty;    my_lwd    <- pp$my_lwd
my_lincol <- as.character(pp$my_lincol)
my_point_col <- as.character(pp$my_point_col)
my_bg     <- as.character(pp$my_bg)
conf_col  <- as.character(pp$conf_col)
plot_type <- as.character(pp$plot_type)
sdi_lower <- as.numeric(as.character(pp$sdi_lower))
sdi_upper <- as.numeric(as.character(pp$sdi_upper))
diff_plot_units <- as.character(pp$diff_plot_units)

# For percent difference, x=y=0 causes division by zero
if (!pp$plot_difference_abs)
  my.data <- subset(my.data, !(my.data$x == my.data$y & my.data$x == 0))

x <- my.data$x; y <- my.data$y

# ── Bland-Altman calculation ──────────────────────────────────────────────────
diff    <- if (pp$plot_difference_abs) y - x else (y-x)/(y+x)*200
sd.diff <- sd(diff); mean.diff <- mean(diff)
BA.ymin <- sdi_lower * sd.diff; BA.ymax <- sdi_upper * sd.diff
mean.of.methods <- (x + y) / 2

ylabel <- if (pp$plot_difference_abs) {
  lbl <- "Difference of Measures"
  if (!is.na(diff_plot_units) && nzchar(diff_plot_units))
    paste0(lbl, " (", diff_plot_units, ")") else lbl
} else "Difference of Measures (%)"

xlabel <- "Average of Measures"
if (!is.na(diff_plot_units) && nzchar(diff_plot_units))
  xlabel <- paste0(xlabel, " (", diff_plot_units, ")")

# Colour helpers
confidence_transp <- round(pp$confidence_transp * 2.55, 0)
class(confidence_transp) <- "hexmode"
cffill <- paste0(col2hex(as.character(pp$confidence_fill_col)),
                 addazero(as.character(confidence_transp)))

# ── Base-R BA plot function ───────────────────────────────────────────────────
BA.plot <- function(x, y) {
  plot(mean.of.methods, diff,
       pch=my_pch, cex=my_cex, lty=my_lty, lwd=my_lwd,
       col=my_point_col, bg=my_bg,
       ylim=c(BA.ymin, BA.ymax),
       xlim=c(min(mean.of.methods), max(mean.of.methods)),
       main="Difference Plot", xlab=xlabel, ylab=ylabel)

  if (pp$plot_confidence) {
    vx <- c(-2,-2,2,2) * max(abs(mean.of.methods))
    vy <- c(mean.diff - 1.96*sd.diff, mean.diff + 1.96*sd.diff,
            mean.diff + 1.96*sd.diff, mean.diff - 1.96*sd.diff)
    polygon(vx, vy, fillOddEven=FALSE, col=cffill, border=NA)
    points(mean.of.methods, diff, pch=my_pch, cex=my_cex,
           col=my_point_col, bg=my_bg, lwd=my_lwd)
  }

  if (pp$plot_conf_outline) {
    abline(h=mean.diff + 1.96*sd.diff, lty=2, col=conf_col, lwd=my_lwd)
    abline(h=mean.diff - 1.96*sd.diff, lty=2, col=conf_col, lwd=my_lwd)
  }

  abline(h=mean.diff, lty=my_lty, col=my_lincol, lwd=my_lwd)
  if (pp$plot_identity) abline(h=0, col="red", lty=2, lwd=my_lwd)

  invisible(list(mean.diff=mean.diff, sd.diff=sd.diff, diff=diff))
}

# ── Static plots ──────────────────────────────────────────────────────────────
dir.create(file.path(tmpdir,"previews"), showWarnings=FALSE)
dir.create(file.path(tmpdir,"plots"),    showWarnings=FALSE)

jpeg(file.path(tmpdir,"previews","B.jpg"),
     width=my_width, height=my_height, units="in", res=my_dpi, pointsize=my_point)
BA <- BA.plot(x, y)
dev.off()

tryCatch({
  open_device(plot_type, file.path(tmpdir,"plots",paste0("BA_plot.",plot_type)),
              my_width, my_height, my_dpi, my_point)
  BA.plot(x, y)
  dev.off()
}, error = function(e) { try(dev.off(), silent=TRUE) })

# ── Write BA statistics once ──────────────────────────────────────────────────
sf <- file.path(tmpdir,"plots","stats_output.txt")
qd <- signif(quantile(BA$diff, probs=c(0.025,0.25,0.50,0.75,0.975)), 4)
if (pp$plot_difference_abs) {
  cat(sprintf("\nMean Difference: %s\nSD Difference: %s\n\nQuantiles of Difference: [2.5,25,50,75,97.5]\n[%s,%s,%s,%s,%s]\n",
              signif(BA$mean.diff,4), signif(BA$sd.diff,4),
              qd[1],qd[2],qd[3],qd[4],qd[5]), file=sf, append=TRUE)
} else {
  cat(sprintf("\nMean Difference: %s%%\nSD Difference: %s%%\n\nQuantiles of Difference(%%): [2.5,25,50,75,97.5]\n[%s,%s,%s,%s,%s]\n",
              signif(BA$mean.diff,4), signif(BA$sd.diff,4),
              qd[1],qd[2],qd[3],qd[4],qd[5]), file=sf, append=TRUE)
}

# ── Interactive plotly widget ─────────────────────────────────────────────────
dir.create(file.path(tmpdir,"widgets"), showWarnings=FALSE)

my.data$avg  <- mean.of.methods
my.data$diff <- diff
my.data$tip  <- paste0("Row: ",  my.data$row,
                        "<br>x: ",  round(my.data$x, 4),
                        "<br>y: ",  round(my.data$y, 4),
                        "<br>Avg: ", round(mean.of.methods, 4),
                        "<br>Diff: ", round(diff, 4))

cf_fill <- as.character(pp$confidence_fill_col)
cf_alph <- pp$confidence_transp / 100
cf_bord <- as.character(pp$conf_col)
lwd_g   <- my_lwd * 0.5
x_pad   <- diff(range(mean.of.methods)) * 0.05
x_range <- c(min(mean.of.methods) - x_pad, max(mean.of.methods) + x_pad)

g <- ggplot(my.data, aes(x=avg, y=diff)) +
  coord_cartesian(ylim=c(BA.ymin, BA.ymax)) +
  labs(x=xlabel, y=ylabel, title="Difference Plot") +
  theme_bw(base_size=my_point) + theme(plot.title=element_text(hjust=0.5))

if (pp$plot_confidence) {
  band_df <- data.frame(avg=c(x_range[1], x_range[2]),
                        ylo=mean.diff - 1.96*sd.diff,
                        yhi=mean.diff + 1.96*sd.diff)
  g <- g + geom_ribbon(data=band_df, aes(x=avg, ymin=ylo, ymax=yhi),
                       inherit.aes=FALSE, fill=cf_fill, alpha=cf_alph, colour=NA)
}
if (pp$plot_conf_outline)
  g <- g +
    geom_hline(yintercept=mean.diff + 1.96*sd.diff,
               colour=cf_bord, linetype="solid", linewidth=lwd_g*1.4) +
    geom_hline(yintercept=mean.diff - 1.96*sd.diff,
               colour=cf_bord, linetype="solid", linewidth=lwd_g*1.4)

g <- g + geom_hline(yintercept=mean.diff, colour=my_lincol, linewidth=lwd_g)
if (pp$plot_identity)
  g <- g + geom_hline(yintercept=0, colour="red", linetype="dashed", linewidth=lwd_g)

g <- g + geom_point(aes(text=tip), shape=my_pch, size=my_cex*2.5,
                    colour=my_point_col, fill=my_bg, stroke=my_lwd*0.4)

x_lab <- x_range[1] + 0.02*diff(x_range)
g <- g +
  annotate("text", x=x_lab, y=mean.diff + 1.96*sd.diff,
           label=paste0("+1.96 SD\n", round(mean.diff+1.96*sd.diff,3)),
           hjust=0, vjust=-0.3, size=3, colour=cf_bord) +
  annotate("text", x=x_lab, y=mean.diff - 1.96*sd.diff,
           label=paste0("-1.96 SD\n", round(mean.diff-1.96*sd.diff,3)),
           hjust=0, vjust=1.2, size=3, colour=cf_bord) +
  annotate("text", x=x_lab, y=mean.diff,
           label=paste0("Mean\n", round(mean.diff,3)),
           hjust=0, vjust=-0.3, size=3, colour=my_lincol)

p_ba <- ggplotly(g, tooltip="text") %>%
  layout(hoverlabel=list(bgcolor="white", font=list(size=12)),
         xaxis=list(range=x_range, autorange=FALSE))

saveWidget(p_ba, file=file.path(tmpdir,"widgets","bland_altman.html"),
           selfcontained=TRUE)
