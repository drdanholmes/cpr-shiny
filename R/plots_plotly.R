#  cp-R: Chemical Pathology R — interactive plotly plots
#  Copyright (C) 2014  Daniel T. Holmes, MD  |  GNU GPL v3
#
#  Built with ggplot2 + ggplotly. Requires packages: ggplot2, plotly.

# Common hover-label styling and top-left annotation box
style_plotly <- function(p, annot = character(0)) {
  if (length(annot))
    p <- p %>% layout(annotations = list(list(
      text = paste(annot, collapse = "<br>"), x = 0.02, xref = "paper",
      y = 0.97, yref = "paper", xanchor = "left", yanchor = "top",
      showarrow = FALSE, bgcolor = "rgba(255,255,255,0.85)",
      bordercolor = "rgba(0,0,0,0.2)", borderwidth = 1, borderpad = 5,
      font = list(size = 13))))
  p %>% layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
}

# ── Regression plot ───────────────────────────────────────────────────────────
plotly_regression <- function(fit, band, pp) {
  d     <- fit$data
  d$tip <- paste0("Row: ", d$row,
                  "<br>x: ", round(d$x, 4),
                  "<br>y: ", round(d$y, 4))
  lwd <- pp$my_lwd * 0.5

  g <- ggplot(d, aes(x = x, y = y)) +
    coord_cartesian(xlim = c(pp$xmin, pp$xmax), ylim = c(pp$ymin, pp$ymax)) +
    labs(x = pp$my_xlab, y = pp$my_ylab, title = pp$my_main) +
    theme_bw(base_size = pp$font_mult) +
    theme(plot.title = element_text(hjust = 0.5))

  if (pp$plot_confidence && !is.null(band)) {
    g <- g + geom_ribbon(data = band, aes(x = x, ymin = lwr, ymax = upr),
                         inherit.aes = FALSE, fill = pp$confidence_fill_col,
                         alpha = pp$confidence_transp / 100, colour = NA)
    if (pp$plot_conf_outline)
      g <- g +
        geom_line(data = band, aes(x = x, y = lwr), inherit.aes = FALSE,
                  colour = pp$conf_col, linetype = "dashed", linewidth = lwd) +
        geom_line(data = band, aes(x = x, y = upr), inherit.aes = FALSE,
                  colour = pp$conf_col, linetype = "dashed", linewidth = lwd)
  }

  g <- g + geom_abline(intercept = fit$intercept, slope = fit$slope,
                       colour = pp$my_lincol, linetype = pp$my_lty, linewidth = lwd)
  if (pp$plot_identity)
    g <- g + geom_abline(intercept = 0, slope = 1, colour = "red",
                         linetype = "dashed", linewidth = lwd)

  g <- g + geom_point(aes(text = tip), shape = pp$my_pch, size = pp$my_cex * 2.5,
                      colour = pp$my_point_col, fill = pp$my_bg,
                      stroke = pp$my_lwd * 0.4)

  annot <- character(0)
  if (pp$plot_regression) annot <- c(annot, equation_string(fit$intercept, fit$slope))
  if (pp$plot_rsquared)   annot <- c(annot, paste0("R²=", round(cor(d$x, d$y)^2, 4)))
  if (pp$plot_method)     annot <- c(annot, paste0("Method: ", fit$method_label))

  style_plotly(ggplotly(g, tooltip = "text"), annot)
}

# ── Bland-Altman plot ─────────────────────────────────────────────────────────
plotly_ba <- function(ba, pp) {
  d      <- ba$data
  d$avg  <- ba$avg
  d$diff <- ba$diff
  d$tip  <- paste0("Row: ",   d$row,
                   "<br>x: ",    round(d$x, 4),
                   "<br>y: ",    round(d$y, 4),
                   "<br>Avg: ",  round(d$avg, 4),
                   "<br>Diff: ", round(d$diff, 4))
  lab   <- ba_axis_labels(ba, pp)
  m     <- ba$mean_diff; s <- ba$sd_diff
  lwd   <- pp$my_lwd * 0.5
  x_pad <- diff(range(d$avg)) * 0.05
  x_rng <- c(min(d$avg) - x_pad, max(d$avg) + x_pad)

  g <- ggplot(d, aes(x = avg, y = diff)) +
    coord_cartesian(ylim = c(pp$sdi_lower * s, pp$sdi_upper * s)) +
    labs(x = lab$x, y = lab$y, title = "Difference Plot") +
    theme_bw(base_size = pp$font_mult) +
    theme(plot.title = element_text(hjust = 0.5))

  if (pp$plot_confidence)
    g <- g + geom_ribbon(data = data.frame(avg = x_rng, ylo = m - 1.96*s, yhi = m + 1.96*s),
                         aes(x = avg, ymin = ylo, ymax = yhi), inherit.aes = FALSE,
                         fill = pp$confidence_fill_col,
                         alpha = pp$confidence_transp / 100, colour = NA)
  if (pp$plot_conf_outline)
    g <- g +
      geom_hline(yintercept = m + 1.96*s, colour = pp$conf_col, linewidth = lwd * 1.4) +
      geom_hline(yintercept = m - 1.96*s, colour = pp$conf_col, linewidth = lwd * 1.4)

  g <- g + geom_hline(yintercept = m, colour = pp$my_lincol, linewidth = lwd)
  if (pp$plot_identity)
    g <- g + geom_hline(yintercept = 0, colour = "red", linetype = "dashed", linewidth = lwd)

  g <- g + geom_point(aes(text = tip), shape = pp$my_pch, size = pp$my_cex * 2.5,
                      colour = pp$my_point_col, fill = pp$my_bg,
                      stroke = pp$my_lwd * 0.4)

  x_lab <- x_rng[1] + 0.02 * diff(x_rng)
  g <- g +
    annotate("text", x = x_lab, y = m + 1.96*s,
             label = paste0("+1.96 SD\n", round(m + 1.96*s, 3)),
             hjust = 0, vjust = -0.3, size = 3, colour = pp$conf_col) +
    annotate("text", x = x_lab, y = m - 1.96*s,
             label = paste0("-1.96 SD\n", round(m - 1.96*s, 3)),
             hjust = 0, vjust = 1.2, size = 3, colour = pp$conf_col) +
    annotate("text", x = x_lab, y = m,
             label = paste0("Mean\n", round(m, 3)),
             hjust = 0, vjust = -0.3, size = 3, colour = pp$my_lincol)

  style_plotly(ggplotly(g, tooltip = "text")) %>%
    layout(xaxis = list(range = x_rng, autorange = FALSE))
}
