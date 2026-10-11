#  cp-R: Chemical Pathology R — static (base graphics) plots
#  Copyright (C) 2014  Daniel T. Holmes, MD  |  GNU GPL v3
#
#  Draw onto whatever device is open; used for the preview and for downloads.
#  `pp` is the plot-parameter list built by app.R.

# ── Helpers ───────────────────────────────────────────────────────────────────
# Colour name/hex -> "#RRGGBBAA" with opacity given in percent (0-100)
fill_with_alpha <- function(col, opacity_pct) {
  v <- col2rgb(col)
  rgb(v[1], v[2], v[3], alpha = round(opacity_pct * 2.55), maxColorValue = 255)
}

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

# "y=1.05x+0.32" / "y=1.05x-0.32"
equation_string <- function(a, b) {
  sprintf("y=%.2fx%s%.2f", b, if (a >= 0) "+" else "-", abs(a))
}

# ── Regression plot ───────────────────────────────────────────────────────────
# band: data.frame(x, lwr, upr) from fit_band(), or NULL
draw_regression <- function(fit, band, pp) {
  x <- fit$data$x; y <- fit$data$y

  plot(x, y,
       pch = pp$my_pch, cex = pp$my_cex, lwd = pp$my_lwd,
       col = pp$my_point_col, bg = pp$my_bg,
       xlim = c(pp$xmin, pp$xmax), ylim = c(pp$ymin, pp$ymax),
       xlab = pp$my_xlab, ylab = pp$my_ylab, main = pp$my_main)

  if (pp$plot_confidence && !is.null(band)) {
    polygon(c(band$x, rev(band$x)), c(band$lwr, rev(band$upr)),
            col = fill_with_alpha(pp$confidence_fill_col, pp$confidence_transp),
            border = NA)
    if (pp$plot_conf_outline) {
      lines(band$x, band$lwr, lty = 2, lwd = pp$my_lwd, col = pp$conf_col)
      lines(band$x, band$upr, lty = 2, lwd = pp$my_lwd, col = pp$conf_col)
    }
  }

  # Replot points (confidence band may have obscured them)
  points(x, y, pch = pp$my_pch, cex = pp$my_cex, lwd = pp$my_lwd,
         col = pp$my_point_col, bg = pp$my_bg)
  abline(fit$intercept, fit$slope,
         lty = pp$my_lty, lwd = pp$my_lwd, col = pp$my_lincol)
  if (pp$plot_identity) abline(0, 1, lwd = pp$my_lwd, lty = 2, col = "red")

  # Annotation text (equation, R², method), stacked from the top-left
  items <- list()
  if (pp$plot_regression) items <- c(items, equation_string(fit$intercept, fit$slope))
  if (pp$plot_rsquared)   items <- c(items, bquote(R^2*"="*.(round(cor(x, y)^2, 4))))
  if (pp$plot_method)     items <- c(items, paste0("Method: ", fit$method_label))
  yspan <- abs(pp$ymax - pp$ymin)
  for (k in seq_along(items))
    text(pp$xmin, pp$ymin + (1.04 - 0.06 * k) * yspan, items[[k]], adj = c(0, 0))
}

# ── Bland-Altman plot ─────────────────────────────────────────────────────────
ba_axis_labels <- function(ba, pp) {
  units <- pp$diff_plot_units
  with_units <- function(lbl) if (nzchar(units)) paste0(lbl, " (", units, ")") else lbl
  list(x = with_units("Average of Measures"),
       y = if (ba$abs_diff) with_units("Difference of Measures")
           else "Difference of Measures (%)")
}

draw_ba <- function(ba, pp) {
  lab <- ba_axis_labels(ba, pp)
  m   <- ba$mean_diff; s <- ba$sd_diff

  plot(ba$avg, ba$diff,
       pch = pp$my_pch, cex = pp$my_cex, lwd = pp$my_lwd,
       col = pp$my_point_col, bg = pp$my_bg,
       ylim = c(pp$sdi_lower * s, pp$sdi_upper * s),
       xlim = range(ba$avg),
       main = "Difference Plot", xlab = lab$x, ylab = lab$y)

  if (pp$plot_confidence) {
    vx <- c(-2, -2, 2, 2) * max(abs(ba$avg))
    vy <- c(m - 1.96*s, m + 1.96*s, m + 1.96*s, m - 1.96*s)
    polygon(vx, vy, fillOddEven = FALSE, border = NA,
            col = fill_with_alpha(pp$confidence_fill_col, pp$confidence_transp))
    points(ba$avg, ba$diff, pch = pp$my_pch, cex = pp$my_cex,
           col = pp$my_point_col, bg = pp$my_bg, lwd = pp$my_lwd)
  }

  if (pp$plot_conf_outline) {
    abline(h = m + 1.96*s, lty = 2, col = pp$conf_col, lwd = pp$my_lwd)
    abline(h = m - 1.96*s, lty = 2, col = pp$conf_col, lwd = pp$my_lwd)
  }

  abline(h = m, lty = pp$my_lty, col = pp$my_lincol, lwd = pp$my_lwd)
  if (pp$plot_identity) abline(h = 0, col = "red", lty = 2, lwd = pp$my_lwd)
}
