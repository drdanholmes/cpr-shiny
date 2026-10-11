#  cp-R: Chemical Pathology R — interactive plotly plots
#  Copyright (C) 2014  Daniel T. Holmes, MD  |  GNU GPL v3
#
#  Built directly with plot_ly() (much faster than ggplotly) and styled to
#  look like ggplot2's theme_bw. Requires package: plotly.

# ── Helpers ───────────────────────────────────────────────────────────────────
PX_PER_MM <- 96 / 25.4

# R pch -> plotly marker symbol
PCH_SYMBOL <- c(
  "0"  = "square-open",        "1"  = "circle-open",
  "2"  = "triangle-up-open",   "3"  = "cross-thin-open",
  "4"  = "x-thin-open",        "5"  = "diamond-open",
  "6"  = "triangle-down-open", "7"  = "square-x-open",
  "8"  = "asterisk-open",      "9"  = "diamond-cross-open",
  "10" = "circle-cross-open",  "11" = "hexagram-open",
  "12" = "square-cross-open",  "13" = "circle-x-open",
  "14" = "square-open",        "15" = "square",
  "16" = "circle",             "17" = "triangle-up",
  "18" = "diamond",            "19" = "circle",
  "20" = "circle",             "21" = "circle",
  "22" = "square",             "23" = "diamond",
  "24" = "triangle-up",        "25" = "triangle-down")

# R lty -> plotly dash
LTY_DASH <- c("solid", "dash", "dot", "dashdot", "longdash", "longdashdot")

col_rgba <- function(col, alpha = 1) {
  v <- col2rgb(col)
  sprintf("rgba(%d,%d,%d,%s)", v[1], v[2], v[3], alpha)
}

# Marker spec for pch 0-25: 21-25 are filled with bg and outlined with col
point_marker <- function(pp) {
  pch    <- as.character(pp$my_pch)
  filled <- pp$my_pch >= 21
  size   <- pp$my_cex * 2.5 * PX_PER_MM * if (pp$my_pch == 20) 2/3 else 1
  list(symbol = unname(PCH_SYMBOL[pch]),
       size   = size,
       color  = col_rgba(if (filled) pp$my_bg else pp$my_point_col),
       line   = list(color = col_rgba(pp$my_point_col),
                     width = max(1, pp$my_lwd)))
}

# ggplot2 default axis expansion: 5% each side
expand5 <- function(lim) lim + c(-1, 1) * 0.05 * diff(lim)

# theme_bw look; base font size in points as in ggplot2
bw_axis <- function(title, range, base) {
  list(title     = list(text = title, font = list(size = base * 4/3)),
       range     = range, autorange = FALSE, zeroline = FALSE,
       showgrid  = TRUE, gridcolor = "rgb(235,235,235)",
       showline  = TRUE, mirror = TRUE, linecolor = "rgb(51,51,51)",
       ticks     = "outside", tickcolor = "rgb(51,51,51)",
       tickfont  = list(size = base * 4/3 * 0.8, color = "rgb(77,77,77)"))
}

bw_layout <- function(p, title, xlab, ylab, xr, yr, base) {
  p %>% layout(
    title         = list(text = title, x = 0.5, xref = "paper",
                         font = list(size = base * 4/3 * 1.2)),
    xaxis         = bw_axis(xlab, xr, base),
    yaxis         = bw_axis(ylab, yr, base),
    showlegend    = FALSE,
    font          = list(color = "black"),
    plot_bgcolor  = "white", paper_bgcolor = "white",
    margin        = list(t = 45, r = 10, b = 45, l = 50),
    hoverlabel    = list(bgcolor = "white", font = list(size = 12)))
}

# Straight line y = a + b x across the x range
add_abline <- function(p, a, b, xr, col, width, dash) {
  add_lines(p, x = xr, y = a + b * xr, hoverinfo = "skip",
            line = list(color = col_rgba(col), width = width, dash = dash))
}

add_band <- function(p, x, lwr, upr, pp) {
  add_polygons(p, x = c(x, rev(x)), y = c(lwr, rev(upr)), hoverinfo = "skip",
               fillcolor = col_rgba(pp$confidence_fill_col, pp$confidence_transp / 100),
               line = list(color = "transparent"))
}

add_points <- function(p, x, y, tip, row, pp) {
  add_markers(p, x = x, y = y, text = tip, hoverinfo = "text",
              customdata = row, marker = point_marker(pp))
}

# ── Regression plot ───────────────────────────────────────────────────────────
plotly_regression <- function(fit, band, pp) {
  d   <- fit$data
  tip <- paste0("Row: ", d$row,
                "<br>x: ", round(d$x, 4),
                "<br>y: ", round(d$y, 4))
  lwd <- pp$my_lwd * 0.5 * PX_PER_MM
  xr  <- expand5(c(pp$xmin, pp$xmax))
  yr  <- expand5(c(pp$ymin, pp$ymax))

  p <- plot_ly(type = "scatter", mode = "markers")
  if (pp$plot_confidence && !is.null(band)) {
    p <- add_band(p, band$x, band$lwr, band$upr, pp)
    if (pp$plot_conf_outline)
      for (yy in list(band$lwr, band$upr))
        p <- add_lines(p, x = band$x, y = yy, hoverinfo = "skip",
                       line = list(color = col_rgba(pp$conf_col), width = lwd, dash = "dash"))
  }
  p <- add_abline(p, fit$intercept, fit$slope, xr, pp$my_lincol, lwd, LTY_DASH[pp$my_lty])
  if (pp$plot_identity) p <- add_abline(p, 0, 1, xr, "red", lwd, "dash")
  p <- add_points(p, d$x, d$y, tip, d$row, pp)

  annot <- character(0)
  if (pp$plot_regression) annot <- c(annot, equation_string(fit$intercept, fit$slope))
  if (pp$plot_rsquared)   annot <- c(annot, paste0("R²=", round(cor(d$x, d$y)^2, 4)))
  if (pp$plot_method)     annot <- c(annot, paste0("Method: ", fit$method_label))

  p <- bw_layout(p, pp$my_main, pp$my_xlab, pp$my_ylab, xr, yr, pp$font_mult)
  if (length(annot))
    p <- p %>% layout(annotations = list(list(
      text = paste(annot, collapse = "<br>"), x = 0.02, xref = "paper",
      y = 0.97, yref = "paper", xanchor = "left", yanchor = "top", align = "left",
      showarrow = FALSE, bgcolor = "rgba(255,255,255,0.85)",
      bordercolor = "rgba(0,0,0,0.2)", borderwidth = 1, borderpad = 5,
      font = list(size = 13))))
  p
}

# ── Bland-Altman plot ─────────────────────────────────────────────────────────
plotly_ba <- function(ba, pp) {
  d   <- ba$data
  tip <- paste0("Row: ",   d$row,
                "<br>x: ",    round(d$x, 4),
                "<br>y: ",    round(d$y, 4),
                "<br>Avg: ",  round(ba$avg, 4),
                "<br>Diff: ", round(ba$diff, 4))
  lab <- ba_axis_labels(ba, pp)
  m   <- ba$mean_diff; s <- ba$sd_diff
  lwd <- pp$my_lwd * 0.5 * PX_PER_MM
  xr  <- expand5(range(ba$avg))   # same as the ±5% padding used before
  yr  <- expand5(c(pp$sdi_lower * s, pp$sdi_upper * s))
  hl  <- function(p, y, col, width, dash = "solid")
    add_lines(p, x = xr, y = c(y, y), hoverinfo = "skip",
              line = list(color = col_rgba(col), width = width, dash = dash))

  p <- plot_ly(type = "scatter", mode = "markers")
  if (pp$plot_confidence)
    p <- add_band(p, xr, rep(m - 1.96*s, 2), rep(m + 1.96*s, 2), pp)
  if (pp$plot_conf_outline)
    for (yy in c(m + 1.96*s, m - 1.96*s)) p <- hl(p, yy, pp$conf_col, lwd * 1.4)
  p <- hl(p, m, pp$my_lincol, lwd)
  if (pp$plot_identity) p <- hl(p, 0, "red", lwd, "dash")
  p <- add_points(p, ba$avg, ba$diff, tip, d$row, pp)

  line_label <- function(y, text, col, above)
    list(x = xr[1] + 0.02 * diff(xr), y = y, text = text, showarrow = FALSE,
         xanchor = "left", yanchor = if (above) "bottom" else "top", align = "left",
         font = list(size = 11, color = col_rgba(col)))

  bw_layout(p, "Difference Plot", lab$x, lab$y, xr, yr, pp$font_mult) %>%
    layout(annotations = list(
      line_label(m + 1.96*s, paste0("+1.96 SD<br>", round(m + 1.96*s, 3)), pp$conf_col,  TRUE),
      line_label(m - 1.96*s, paste0("-1.96 SD<br>", round(m - 1.96*s, 3)), pp$conf_col,  FALSE),
      line_label(m,          paste0("Mean<br>",     round(m, 3)),          pp$my_lincol, TRUE)))
}
