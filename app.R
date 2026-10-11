#    cp-R Shiny: A Shiny Web Interface to R for Clinical Chemists
#    Migrated from the PyQt4 original by Daniel T. Holmes, MD
#    GNU GPL v3 — see gpl-3.0_license.pdf
#
#    "Buy the truth but never sell it" Proverbs 23:23

# ── 1. Install / load R packages ─────────────────────────────────────────────
local({
  needed  <- c("shiny", "rhandsontable", "shinyjs", "jsonlite",
               "plotly", "boot", "mcr")
  missing <- needed[!(needed %in% installed.packages()[, "Package"])]
  if (length(missing))
    install.packages(missing, repos = "https://cloud.r-project.org")
})

library(shiny)
library(rhandsontable)
library(shinyjs)
library(plotly)
library(boot)
suppressPackageStartupMessages(library(mcr))

# ── 2. Analysis and plotting functions ───────────────────────────────────────
# Shiny sources R/*.R automatically before app.R; nothing to do here.

# ── 3. UI ─────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$title("cp-R: Chemical Pathology R"),
    tags$style(HTML("
      /* ── Global reset & body ───────────────────────────────────────────── */
      * { box-sizing: border-box; }
      body {
        font-size: 13px;
        background: linear-gradient(135deg, #f0f4f8 0%, #e8edf2 100%);
        min-height: 100vh;
        font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
      }

      /* ── Page header ───────────────────────────────────────────────────── */
      .cpr-header {
        background: linear-gradient(90deg, #1c1c2e 0%, #2e2e48 50%, #1c1c2e 100%);
        color: #fff;
        padding: 10px 18px;
        margin-bottom: 14px;
        border-radius: 8px;
        display: flex;
        align-items: center;
        gap: 12px;
        box-shadow: 0 3px 10px rgba(0,0,0,.28);
        letter-spacing: .01em;
      }
      .cpr-header strong { font-size: 19px; font-weight: 700; letter-spacing: .03em; }
      .cpr-header .sub   { font-size: 12.5px; opacity: .85; }
      .cpr-header .byline{ margin-left: auto; font-size: 11px; opacity: .5; }

      /* ── Column containers ─────────────────────────────────────────────── */
      /* Give the data-entry column some of the middle column's width so the
         table's exclude column fits on laptop screens (was 25% / 25% / 50%) */
      @media (min-width: 768px) {
        .cpr-col-left { width: 28%; }
        .cpr-col-mid  { width: 22%; }
      }

      /* Each column gets one outer card that fills the column */
      .col-card {
        border-radius: 10px;
        padding: 12px 12px 14px;
        margin-bottom: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,.10);
        height: 100%;
      }

      /* LEFT  — steel blue */
      .col-left  {
        background: linear-gradient(160deg, #edf3f8 0%, #d8eaf4 100%);
        border: 1.5px solid #a8c8e0;
      }
      /* MIDDLE — soft lavender */
      .col-mid   {
        background: linear-gradient(160deg, #f2eef9 0%, #e4daf4 100%);
        border: 1.5px solid #c4a8e8;
      }
      /* RIGHT  — cool charcoal */
      .col-right {
        background: linear-gradient(160deg, #f0f0f4 0%, #e2e2ea 100%);
        border: 1.5px solid #b8b8cc;
      }

      /* ── Section headers (h5) — tinted per column ──────────────────────── */
      .col-left  h5 {
        color: #1c3f5e;
        border-bottom: 2px solid #a8c8e0;
        padding-bottom: 3px;
        margin: 10px 0 6px;
        font-size: 12.5px;
        text-transform: uppercase;
        letter-spacing: .06em;
      }
      .col-mid h5 {
        color: #4a2e8a;
        border-bottom: 2px solid #c4a8e8;
        padding-bottom: 3px;
        margin: 10px 0 6px;
        font-size: 12.5px;
        text-transform: uppercase;
        letter-spacing: .06em;
      }
      .col-right h5 {
        color: #2c2c3e;
        border-bottom: 2px solid #b8b8cc;
        padding-bottom: 3px;
        margin: 10px 0 6px;
        font-size: 12.5px;
        text-transform: uppercase;
        letter-spacing: .06em;
      }

      /* ── Inner section panels ───────────────────────────────────────────── */
      .panel-inner {
        background: rgba(255,255,255,.65);
        border-radius: 7px;
        padding: 9px 10px 10px;
        margin-bottom: 9px;
        backdrop-filter: blur(2px);
        position: relative;
      }
      /* backdrop-filter gives each panel its own stacking layer, so an open
         dropdown would sit under the next panel; lift the focused panel */
      .panel-inner:focus-within { z-index: 10; }
      .col-left  .panel-inner { border: 1px solid rgba(92,157,192,.35); }
      .col-mid   .panel-inner { border: 1px solid rgba(160,127,214,.35); }
      .col-right .panel-inner { border: 1px solid rgba(136,136,160,.35); }

      /* ── Form controls ──────────────────────────────────────────────────── */
      .compact .form-group { margin-bottom: 6px; }
      .compact label       { font-size: 11.5px; font-weight: 600;
                             color: #444; margin-bottom: 1px; }
      .form-control {
        font-size: 12.5px;
        border-radius: 5px;
        border: 1px solid #ccc;
        padding: 3px 7px;
        height: 28px;
        transition: border-color .15s, box-shadow .15s;
      }
      .form-control:focus {
        outline: none;
        border-color: #8877cc;
        box-shadow: 0 0 0 3px rgba(130,110,200,.18);
      }
      /* Radio / checkbox labels */
      .radio label, .checkbox label { font-size: 12.5px; }

      /* ── Buttons ────────────────────────────────────────────────────────── */
      /* Left column — steel blue theme */
      .col-left  .btn-default {
        background: #edf3f8; border-color: #a8c8e0; color: #1c3f5e;
        border-radius: 5px; font-size: 12px;
      }
      .col-left  .btn-default:hover { background: #d8eaf4; border-color: #5c9dc0; }
      .col-left  .btn-danger  { border-radius: 5px; font-size: 12px; }
      .col-left  .btn-success {
        background: linear-gradient(135deg, #2e86c1, #155278);
        border-color: #0e4060; border-radius: 5px; font-size: 12px;
        color: #fff; font-weight: 600;
        box-shadow: 0 2px 6px rgba(20,80,130,.35);
      }
      .col-left  .btn-success:hover {
        background: linear-gradient(135deg, #1f6898, #0e4060);
        box-shadow: 0 3px 9px rgba(20,80,130,.45);
      }

      /* Right column — charcoal theme */
      .col-right .btn-default {
        background: #f0f0f4; border-color: #b8b8cc; color: #2c2c3e;
        border-radius: 5px; font-size: 12px;
      }
      .col-right .btn-default:hover { background: #e2e2ea; border-color: #8888a0; }
      .col-right .btn-info {
        background: linear-gradient(135deg, #6942b8, #4a2e8a);
        border-color: #3a2070; border-radius: 5px; font-size: 12px; color: #fff;
        box-shadow: 0 2px 5px rgba(90,50,160,.3);
      }
      .col-right .btn-info:hover {
        background: linear-gradient(135deg, #5535a0, #3a2070);
      }
      .col-right .btn-primary {
        background: linear-gradient(135deg, #3c3c52, #26263a);
        border-color: #1a1a28; border-radius: 5px; font-size: 12px; color: #fff;
        box-shadow: 0 2px 5px rgba(30,30,50,.35);
      }
      .col-right .btn-primary:hover {
        background: linear-gradient(135deg, #2c2c42, #1a1a28);
      }

      /* ── Handsontable wrapper ───────────────────────────────────────────── */
      .hot-wrap {
        border: 1.5px solid #a8c8e0;
        border-radius: 6px;
        overflow: hidden;
        box-shadow: inset 0 1px 3px rgba(0,0,0,.06);
      }

      /* ── Plot preview area ─────────────────────────────────────────────── */
      .plot-frame {
        text-align: center;
        min-height: 480px;
        background: linear-gradient(145deg, #f0f0f4, #e0e0ec);
        border: 1.5px solid #b8b8cc;
        border-radius: 7px;
        padding: 5px;
        display: flex;
        align-items: center;
        justify-content: center;
        box-shadow: inset 0 1px 5px rgba(0,0,0,.07);
      }
      #plot_placeholder { color: #8888a0; font-size: 13px; padding-top: 140px; }

      /* ── Plot navigation bar ───────────────────────────────────────────── */
      .plot-nav {
        display: flex;
        align-items: center;
        gap: 5px;
        justify-content: center;
        margin-top: 8px;
        flex-wrap: wrap;
      }
      .plot-nav .shiny-download-link,
      .plot-nav .btn { padding: 3px 10px; font-size: 11.5px; border-radius: 5px; }
      .plot-nav-idx { font-size: 12px; color: #6942b8; font-weight: 600;
                      min-width: 40px; text-align: center; }

      /* ── Stats box ─────────────────────────────────────────────────────── */
      .stats-box {
        font-family: 'Consolas', 'Courier New', monospace;
        font-size: 11.5px;
        white-space: pre-wrap;
        background: rgba(255,255,255,.78);
        border: 1.5px solid #b8b8cc;
        padding: 9px 10px;
        height: 360px;
        overflow-y: auto;
        border-radius: 7px;
        color: #1e1e2e;
        line-height: 1.55;
        box-shadow: inset 0 1px 4px rgba(0,0,0,.06);
      }
      .stats-wrap { position: relative; }
      .stats-copy {
        position: absolute;
        top: 6px;
        right: 18px;
        z-index: 2;
        opacity: .7;
      }
      .stats-copy:hover { opacity: 1; }

      /* ── Confidence band sub-panel ─────────────────────────────────────── */
      .conf-sub {
        background: rgba(255,255,255,.5);
        border: 1px solid rgba(160,127,214,.5);
        border-radius: 5px;
        padding: 7px 8px;
        margin-top: 5px;
      }

      /* ── Graph button: table changed since the last run ─────────────────── */
      .col-left .btn-success.btn-pending {
        background: linear-gradient(135deg, #e67e22, #b85c0e);
        border-color: #8e4708;
        box-shadow: 0 0 0 3px rgba(230,126,34,.35);
      }

      /* ── pch / lty pickers ─────────────────────────────────────────────── */
      .glyph-opt { display: flex; align-items: center; gap: 6px; }
      .glyph-opt svg { flex-shrink: 0; }
      .selectize-input .glyph-opt { display: inline-flex; vertical-align: middle; }
      .selectize-input.has-items input { position: absolute; width: 4px !important; }
      .compact .selectize-input { min-height: 28px; padding: 3px 7px; font-size: 12.5px; }
      /* Narrow middle column: less side padding so values aren't clipped */
      .compact .form-control { padding-left: 6px; padding-right: 6px; }
      .compact .row { margin-left: -4px; margin-right: -4px; }
      .compact .row > [class*='col-'] { padding-left: 4px; padding-right: 4px; }
      .compact .selectize-dropdown .option { padding: 2px 7px; }

      /* ── Color picker widgets ───────────────────────────────────────────── */
      .color-row { display: flex; align-items: center; gap: 4px; }
      .color-swatch {
        width: 22px; height: 22px; min-width: 22px;
        border-radius: 4px;
        border: 1px solid rgba(0,0,0,.2);
        background: #fff;
        display: inline-block; flex-shrink: 0;
        margin-top: 1px;
        transition: background .15s;
        box-shadow: 0 1px 3px rgba(0,0,0,.15);
      }
      .color-invalid input.form-control {
        border-color: #e74c3c !important;
        box-shadow: 0 0 0 2px rgba(231,76,60,.25) !important;
      }
      .color-invalid .color-swatch {
        background: repeating-linear-gradient(
          45deg,#e74c3c,#e74c3c 3px,#fff 3px,#fff 7px
        ) !important;
      }
    ")),
    # Inline JS: live swatch update + datalist injection for all color inputs
    tags$script(HTML(sprintf("
      // R colour name -> hex (many R names, e.g. gray50, are not CSS colours)
      var R_COLORS = %s;

      // Build one shared <datalist> and attach to every color input
      document.addEventListener('DOMContentLoaded', function() {
        var dl = document.createElement('datalist');
        dl.id  = 'r-color-list';
        Object.keys(R_COLORS).forEach(function(c) {
          var opt = document.createElement('option');
          opt.value = c;
          dl.appendChild(opt);
        });
        document.body.appendChild(dl);

        // Attach datalist + live swatch behaviour to all color inputs
        ['col','bg','lincol','conf_fill_col','conf_col'].forEach(function(id) {
          attachColor(id);
        });
      });

      function attachColor(id) {
        // textInput() puts the id on the <input> itself
        var inp = document.getElementById(id);
        if (!inp) { setTimeout(function(){ attachColor(id); }, 200); return; }

        // Link datalist
        inp.setAttribute('list', 'r-color-list');
        inp.setAttribute('autocomplete', 'off');

        // Put the input and a swatch side by side
        var row = document.createElement('div');
        row.className = 'color-row';
        inp.before(row);
        row.appendChild(inp);
        var swatch = document.createElement('span');
        swatch.className = 'color-swatch';
        swatch.id = 'swatch-' + id;
        row.appendChild(swatch);

        // Live update
        inp.addEventListener('input',  function(){ updateSwatch(id, inp.value); });
        inp.addEventListener('change', function(){ updateSwatch(id, inp.value); });
        updateSwatch(id, inp.value);
      }

      function updateSwatch(id, val) {
        var swatch = document.getElementById('swatch-' + id);
        if (!swatch) return;
        var wrap = document.getElementById(id).closest('.form-group');
        var css = /^#[0-9a-fA-F]{3,8}$/.test(val.trim()) ? val.trim()
                  : R_COLORS[val.trim().toLowerCase()];
        if (css) {
          swatch.style.background = css;
          // Tint the pch / lty picker glyphs with the chosen colours
          document.documentElement.style.setProperty('--sw-' + id, css);
          if (wrap) wrap.classList.remove('color-invalid');
        } else {
          swatch.style.background = '';
          if (wrap) {
            if (val.trim().length > 0) wrap.classList.add('color-invalid');
            else                       wrap.classList.remove('color-invalid');
          }
        }
      }
    ", jsonlite::toJSON(as.list(setNames(
         rgb(t(col2rgb(colors())), maxColorValue = 255), tolower(colors()))),
       auto_unbox = TRUE)))),
    # Inline JS: SVG glyphs for the point-style (pch) and line-type (lty)
    # pickers, used as selectize render functions
    tags$script(HTML("
      var CPR_COL = 'var(--sw-col, blue)', CPR_BG = 'var(--sw-bg, gray)';

      function cprShape(kind, fill) {
        var st = ' style=\"stroke:' + CPR_COL + ';stroke-width:1.4;fill:' + (fill || 'none') + '\"';
        switch (kind) {
          case 'circle':  return '<circle cx=\"9\" cy=\"9\" r=\"5.5\"' + st + '/>';
          case 'small':   return '<circle cx=\"9\" cy=\"9\" r=\"3\"' + st + '/>';
          case 'square':  return '<rect x=\"3.5\" y=\"3.5\" width=\"11\" height=\"11\"' + st + '/>';
          case 'up':      return '<polygon points=\"9,2.5 15.5,14.5 2.5,14.5\"' + st + '/>';
          case 'down':    return '<polygon points=\"2.5,3.5 15.5,3.5 9,15.5\"' + st + '/>';
          case 'diamond': return '<polygon points=\"9,2 16,9 9,16 2,9\"' + st + '/>';
          case 'sdiam':   return '<polygon points=\"9,4 14,9 9,14 4,9\"' + st + '/>';
          case 'plus':    return '<path d=\"M9 2.5V15.5M2.5 9H15.5\"' + st + '/>';
          case 'x':       return '<path d=\"M4 4L14 14M14 4L4 14\"' + st + '/>';
          case 'roof':    return '<path d=\"M3.5 14.5L9 3.5L14.5 14.5\"' + st + '/>';
        }
      }

      var CPR_PCH = {
        0: [['square']],  1: [['circle']], 2: [['up']], 3: [['plus']], 4: [['x']],
        5: [['diamond']], 6: [['down']],
        7: [['square'], ['x']],      8: [['plus'], ['x']],
        9: [['diamond'], ['plus']], 10: [['circle'], ['plus']],
        11: [['up'], ['down']],     12: [['square'], ['plus']],
        13: [['circle'], ['x']],    14: [['square'], ['roof']],
        15: [['square', CPR_COL]],  16: [['circle', CPR_COL]], 17: [['up', CPR_COL]],
        18: [['sdiam', CPR_COL]],   19: [['circle', CPR_COL]], 20: [['small', CPR_COL]],
        21: [['circle', CPR_BG]],   22: [['square', CPR_BG]],  23: [['diamond', CPR_BG]],
        24: [['up', CPR_BG]],       25: [['down', CPR_BG]]
      };

      function cprPchRender(item, escape) {
        var parts = CPR_PCH[item.value] || [];
        var svg = '<svg width=\"18\" height=\"18\" viewBox=\"0 0 18 18\">' +
          parts.map(function(p) { return cprShape(p[0], p[1]); }).join('') + '</svg>';
        return '<div class=\"glyph-opt\">' + svg + '<span>' + escape(item.label) + '</span></div>';
      }

      // R lty dash patterns, scaled for a 2px line
      var CPR_LTY = { 1: '', 2: '8,8', 3: '2,6', 4: '2,6,8,6', 5: '14,6', 6: '4,4,12,4' };

      function cprLtyRender(item, escape) {
        var dash = CPR_LTY[item.value] ? ' stroke-dasharray=\"' + CPR_LTY[item.value] + '\"' : '';
        var svg = '<svg width=\"32\" height=\"10\" viewBox=\"0 0 32 10\">' +
          '<line x1=\"1\" y1=\"5\" x2=\"31\" y2=\"5\" style=\"stroke:var(--sw-lincol, black);stroke-width:2\"' +
          dash + '/></svg>';
        return '<div class=\"glyph-opt\">' + svg + '<span>' + escape(item.label) + '</span></div>';
      }

      // Copy the statistical output to the clipboard; falls back to
      // execCommand where the async clipboard API is unavailable (plain http)
      function cprCopyStats(btn) {
        var el = document.getElementById('stats_box');
        var text = el ? el.innerText : '';
        var done = function() {
          var icon = btn.querySelector('i');
          var orig = icon.className;
          icon.className = 'fas fa-check';
          btn.title = 'Copied!';
          setTimeout(function() { icon.className = orig; btn.title = 'Copy to clipboard'; }, 1500);
        };
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(text).then(done);
        } else {
          var ta = document.createElement('textarea');
          ta.value = text;
          ta.style.position = 'fixed';
          ta.style.opacity = '0';
          document.body.appendChild(ta);
          ta.select();
          try { if (document.execCommand('copy')) done(); } finally { document.body.removeChild(ta); }
        }
      }
    ")),

    # Inline JS: stretch the data-entry table so the left column ends level
    # with the middle column. Only when the columns sit side by side (>= 768px);
    # never shorter than the default 488px.
    tags$script(HTML("
      (function() {
        var DEFAULT_H = 488, pending = null;

        function fitTable() {
          var el   = document.getElementById('hot_table'),
              left = document.querySelector('.col-left'),
              mid  = document.querySelector('.col-mid'),
              w    = window.HTMLWidgets && HTMLWidgets.find('#hot_table'),
              hot  = w && w.hot;
          if (!el || !left || !mid || !hot) return;

          var h = DEFAULT_H;
          if (window.innerWidth >= 768) {
            // container is 2px taller than the table itself
            var target = el.offsetHeight + (mid.offsetHeight - left.offsetHeight) - 2;
            h = Math.max(DEFAULT_H, Math.round(target));
          }
          if (h === hot.getSettings().height) return;

          el.style.height = (h + 2) + 'px';
          hot.updateSettings({ height: h });
          if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('hot_height', h);
        }

        function schedule() {
          if (pending) return;
          pending = requestAnimationFrame(function() { pending = null; fitTable(); });
        }

        $(function() {
          var mid = document.querySelector('.col-mid');
          if (mid && window.ResizeObserver) new ResizeObserver(schedule).observe(mid);
          $(window).on('resize', schedule);
        });
        $(document).on('shiny:value', function(e) {
          if (e.name === 'hot_table') setTimeout(schedule, 50);
        });
      })();
    "))
  ),

  # ── Header ─────────────────────────────────────────────────────────────────
  div(class = "cpr-header",
    tags$strong("cp-R"),
    tags$span(class = "sub",
              "Chemical Pathology R \u2014 Regression Analysis (Shiny Edition)"),
    tags$span(class = "byline",
              "Dr. Daniel T. Holmes MD | Shiny port")
  ),

  fluidRow(

    # ────────────────────────────────────────────────────────────────────────
    # LEFT COLUMN: data-manipulation buttons + rhandsontable
    # ────────────────────────────────────────────────────────────────────────
    column(3, class = "cpr-col-left",
      div(class = "col-card col-left",
        h5("Data Entry  (x / y)"),
        fluidRow(
          column(6,
            actionButton("btn_clear",  "Clear",     icon("trash"),
                         class = "btn btn-danger btn-sm btn-block")
          ),
          column(6,
            actionButton("btn_paste",  "Paste Data", icon("paste"),
                         class = "btn btn-default btn-sm btn-block")
          )
        ),
        fluidRow(style = "margin-top:4px;",
          column(6,
            actionButton("btn_sort_x", "Sort by x", icon("sort-numeric-asc"),
                         class = "btn btn-default btn-sm btn-block")
          ),
          column(6,
            actionButton("btn_sort_y", "Sort by y", icon("sort-numeric-asc"),
                         class = "btn btn-default btn-sm btn-block")
          )
        ),
        fluidRow(style = "margin-top:4px;",
          column(6,
            actionButton("btn_swap",   "Swap x/y",  icon("exchange"),
                         class = "btn btn-default btn-sm btn-block")
          ),
          column(6,
            actionButton("btn_graph",  "\u25b6  Graph",
                         class = "btn btn-success btn-sm btn-block")
          )
        ),
        div(class = "hot-wrap", style = "margin-top:10px;",
          rHandsontableOutput("hot_table", height = "490px")
        )
      )
    ),

    # ────────────────────────────────────────────────────────────────────────
    # MIDDLE COLUMN: all graphical / analysis parameters
    # ────────────────────────────────────────────────────────────────────────
    column(3, class = "cpr-col-mid",
      div(class = "col-card col-mid compact",

        # ── Point & Line style ────────────────────────────────────────────
        div(class = "panel-inner",
          h5("Point & Line Style"),
          fluidRow(
            column(6, selectizeInput("pointstyle", "Pt Style (pch)",
                                     choices = 0:25, selected = 21,
                                     options = list(render = I(
                                       "{ option: cprPchRender, item: cprPchRender }")))),
            column(6, numericInput("pointcex",   "Pt Magnif.",      1, 0.1, 5, 0.1))
          ),
          fluidRow(
            column(6, textInput("col", "Pt Color", "blue")),
            column(6, textInput("bg",  "Pt Fill",  "gray"))
          ),
          fluidRow(
            column(6, selectizeInput("lty", "Line Style (lty)",
                                     choices = 1:6, selected = 1,
                                     options = list(render = I(
                                       "{ option: cprLtyRender, item: cprLtyRender }")))),
            column(6, numericInput("lwd",    "Line Width",       1, 0.5, 10, 0.5))
          ),
          fluidRow(
            column(6, textInput   ("lincol",          "Line Color", "black")),
            column(6, numericInput("font_multiplier", "Font Size",  12, 6, 30, 1))
          )
        ),

        # ── Axis labels & plot limits ─────────────────────────────────────
        div(class = "panel-inner",
          h5("Labels & Axis Limits"),
          textInput("xlab", "x-axis label", "x"),
          textInput("ylab", "y-axis label", "y"),
          textInput("main", "Main title",   "Regression Plot"),
          fluidRow(
            column(6, textInput("xmin", "xmin", placeholder = "auto")),
            column(6, textInput("xmax", "xmax", placeholder = "auto"))
          ),
          fluidRow(
            column(6, textInput("ymin", "ymin", placeholder = "auto")),
            column(6, textInput("ymax", "ymax", placeholder = "auto"))
          )
        ),

        # ── Regression method ─────────────────────────────────────────────
        div(class = "panel-inner",
          h5("Method of Regression"),
          radioButtons("reg_method", NULL,
            choices  = c("Least Squares"  = "LS",
                         "Passing-Bablok" = "PB",
                         "Deming"         = "Deming"),
            selected = "LS"),
          conditionalPanel("input.reg_method === 'LS'",
            checkboxInput("weighting_lin", "Weighted (1/x\u00b2)", FALSE)
          ),
          conditionalPanel("input.reg_method === 'Deming'",
            checkboxInput("weighting_dem", "Weighted (1/x\u00b2)", FALSE),
            numericInput("deming_ratio", "Var(y)/Var(x)", 1, min = 0.001, step = 0.1)
          )
        ),

        # ── Plot overlays ─────────────────────────────────────────────────
        div(class = "panel-inner",
          h5("Plot Overlays"),
          checkboxInput("chk_regression", "Show regression equation", TRUE),
          checkboxInput("chk_method",     "Show regression method",   TRUE),
          checkboxInput("chk_rsquared",   "Show R\u00b2",             TRUE),
          checkboxInput("chk_identity",   "Line of identity",         FALSE),
          checkboxInput("chk_confidence", "Confidence band",          TRUE),
          conditionalPanel("input.chk_confidence == true",
            div(class = "conf-sub",
              textInput   ("conf_fill_col", "Fill color",  "lightslateblue"),
              fluidRow(
                column(6, numericInput("conf_transp", "Opacity (%)", 30, 0, 100, 5)),
                column(6, textInput   ("conf_col",    "Outline col", "blue"))
              ),
              checkboxInput("chk_conf_outline", "Draw outline", FALSE)
            )
          )
        ),

        # ── Difference (Bland-Altman) plot ────────────────────────────────
        div(class = "panel-inner",
          h5("Difference Plot"),
          radioButtons("diff_style", "Style",
            choices  = c("Absolute" = "abs", "Percent (%)" = "perc"),
            selected = "perc", inline = TRUE),
          fluidRow(
            column(6, numericInput("sdi_lower", "SDI lower", -4, step = 0.5)),
            column(6, numericInput("sdi_upper", "SDI upper",  4, step = 0.5))
          ),
          textInput("diff_plot_units", "Units (optional)", "")
        ),

        # ── Output image settings ─────────────────────────────────────────
        div(class = "panel-inner",
          h5("Output Image"),
          radioButtons("plot_type", "Format",
            choices  = c("jpg", "png", "tiff", "bmp", "pdf", "ps"),
            selected = "jpg", inline = TRUE),
          fluidRow(
            column(6, numericInput("img_height", "Height (in)", 6, 1, 20, 0.5)),
            column(6, numericInput("img_width",  "Width (in)",  6, 1, 20, 0.5))
          ),
          numericInput("img_dpi", "DPI", 300, 300, 300, 300)
        )

      )
    ),

    # ────────────────────────────────────────────────────────────────────────
    # RIGHT COLUMN: plot previews + statistical output
    # ────────────────────────────────────────────────────────────────────────
    column(6,
      div(class = "col-card col-right",

        div(class = "panel-inner",
          h5("Plot Preview"),
          div(class = "plot-frame",
            uiOutput("plot_preview")
          ),
          div(class = "plot-nav",
            actionButton("btn_prev", "\u25c0 Prev",
                         class = "btn btn-default btn-sm"),
            tags$span(class = "plot-nav-idx",
                      textOutput("plot_index_label", inline = TRUE)),
            actionButton("btn_next", "Next \u25b6",
                         class = "btn btn-default btn-sm"),
            downloadButton("dl_current_plot", "\u2b07 Current",
                           class = "btn btn-info btn-sm"),
            downloadButton("dl_all_plots", "\u2b07 All",
                           class = "btn btn-primary btn-sm")
          )
        ),

        div(class = "panel-inner",
          h5("Interactive Plot"),
          plotlyOutput("interactive_plot", height = "480px")
        ),

        div(class = "panel-inner",
          h5("Statistical Output"),
          div(class = "stats-wrap",
            tags$button(type = "button", class = "btn btn-default btn-xs stats-copy",
                        title = "Copy to clipboard",
                        onclick = "cprCopyStats(this)", icon("copy")),
            div(class = "stats-box",
              verbatimTextOutput("stats_box")
            )
          )
        )

      )
    )

  )
)


# ── 4. Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Gray out and strike through the x / y cells of excluded rows
  EXCLUDED_ROW_RENDERER <- "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);
      var excluded = instance.getDataAtCell(row, 2) === true;
      td.style.color          = excluded ? '#a0a0a0' : '';
      td.style.background     = excluded ? '#eeeeee' : '';
      td.style.textDecoration = excluded ? 'line-through' : '';
    }"

  INIT_ROWS <- 50

  # Blank table rows; `exclude` = leave the row out of the analysis
  blank_rows <- function(n) {
    data.frame(x = rep(NA_real_, n), y = rep(NA_real_, n), exclude = rep(FALSE, n))
  }

  rv <- reactiveValues(
    df     = blank_rows(INIT_ROWS),
    result = NULL,   # fit, band, ba, pp, stats, points from the last Graph run
    pidx   = 1L
  )

  # ── rhandsontable ─────────────────────────────────────────────────────────
  output$hot_table <- renderRHandsontable({
    # Keep the height the browser last fitted (see the stretch script in the
    # UI); isolate() so resizing never re-renders the table
    hot_h <- isolate(input$hot_height)
    if (is.null(hot_h)) hot_h <- 488
    rhandsontable(
      rv$df,
      rowHeaders  = seq_len(nrow(rv$df)),
      colHeaders  = c("x", "y", "exclude"),
      stretchH    = "all",
      height      = hot_h,
      contextMenu = TRUE,
      overflow    = "hidden"
    ) %>%
      hot_col("x", type = "numeric", format = "0.[000]", renderer = EXCLUDED_ROW_RENDERER) %>%
      hot_col("y", type = "numeric", format = "0.[000]", renderer = EXCLUDED_ROW_RENDERER) %>%
      hot_col("exclude", type = "checkbox", halign = "htCenter") %>%
      hot_cols(colWidths = c(85, 85, 60))
  })

  # Table contents as a data.frame. hot_to_r() flattens each row into one
  # vector, so the checkbox column can come back as 1/0 or "TRUE"/"FALSE";
  # coerce it back to logical (blank = not excluded).
  read_table <- function() {
    d <- hot_to_r(input$hot_table)
    ex <- if (is.null(d$exclude)) FALSE else d$exclude
    ex <- if (is.character(ex)) toupper(ex) == "TRUE" else as.logical(ex)
    ex[is.na(ex)] <- FALSE
    d$exclude <- rep_len(ex, nrow(d))
    d
  }

  # Sync table edits back to rv$df
  observe({
    req(input$hot_table)
    rv$df <- read_table()
  })

  # ── Clear ─────────────────────────────────────────────────────────────────
  observeEvent(input$btn_clear, {
    rv$df <- blank_rows(min_rows())
  })

  # ── Paste via modal ───────────────────────────────────────────────────────
  observeEvent(input$btn_paste, {
    showModal(modalDialog(
      title = "Paste Data",
      tags$p("Copy two-column (x TAB y) data from a spreadsheet, paste below, click OK."),
      tags$p(tags$em("Tip: you can also paste directly into the table cells using Ctrl+V / Cmd+V.")),
      tags$textarea(
        id          = "paste_area",
        style       = "width:100%; height:220px; font-family:monospace; font-size:12px;",
        placeholder = "x[TAB]y  — one pair per line"
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_paste_ok", "OK", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$btn_paste_ok, {
    raw <- input$paste_area
    removeModal()
    req(nzchar(trimws(raw)))

    lines  <- strsplit(trimws(raw), "[\r\n]+")[[1]]
    lines  <- lines[nzchar(trimws(lines))]
    parsed <- lapply(lines, function(ln) {
      parts <- strsplit(ln, "\t")[[1]]
      parts <- gsub(",", ".", parts)        # French locale decimals
      xv <- suppressWarnings(as.numeric(parts[1]))
      yv <- if (length(parts) >= 2) suppressWarnings(as.numeric(parts[2])) else NA_real_
      c(xv, yv)
    })
    mat    <- do.call(rbind, parsed)
    new_df <- as.data.frame(mat, stringsAsFactors = FALSE)
    names(new_df) <- c("x", "y")
    new_df$exclude <- FALSE
    rv$df <- pad_df(new_df)
  })

  # ── Sort / Swap ───────────────────────────────────────────────────────────
  get_clean <- function() {
    d <- rv$df
    d[!(is.na(d$x) & is.na(d$y)), ]
  }

  # Rows needed to fill the stretched table (header ~26px, rows 23px), so no
  # empty space shows below the last row; never fewer than INIT_ROWS
  min_rows <- function() {
    h <- isolate(input$hot_height)
    if (is.null(h)) return(INIT_ROWS)
    max(INIT_ROWS, ceiling((h - 26) / 23))
  }

  # Table grew taller: top up with blank rows
  observeEvent(input$hot_height, {
    if (nrow(rv$df) < min_rows()) rv$df <- pad_df(rv$df)
  })

  pad_df <- function(d) {
    pad <- max(0L, min_rows() - nrow(d))
    if (pad > 0) d <- rbind(d, blank_rows(pad))
    d
  }

  observeEvent(input$btn_sort_x, {
    rv$df <- pad_df(get_clean()[order(get_clean()$x, na.last = TRUE), ])
  })

  observeEvent(input$btn_sort_y, {
    rv$df <- pad_df(get_clean()[order(get_clean()$y, na.last = TRUE), ])
  })

  observeEvent(input$btn_swap, {
    d <- rv$df; tmp <- d$x; d$x <- d$y; d$y <- tmp; rv$df <- d
  })

  # ── GRAPH (main analysis) ─────────────────────────────────────────────────
  observeEvent(input$btn_graph, {

    # Sync last table edit
    if (!is.null(input$hot_table)) rv$df <- read_table()

    # Remove any row where EITHER x or y is NA so that the row numbers
    # displayed in the handsontable exactly match the Row: N tooltip in plotly.
    # Blank rows are added back after the data so the table stays full height.
    complete_mask <- !is.na(rv$df$x) & !is.na(rv$df$y)
    dat <- rv$df[complete_mask, , drop = FALSE]
    rownames(dat) <- NULL   # reset to 1, 2, 3, ... to match plotly
    rv$df <- pad_df(dat)

    if (sum(!dat$exclude) < 3) {
      showNotification("Please enter at least 3 complete (x, y) pairs that are not excluded.",
                       type = "warning")
      return()
    }

    # ── Validate color inputs ──────────────────────────────────────────────
    valid_colors <- tolower(colors())
    is_valid_color <- function(s) {
      s <- trimws(s)
      tolower(s) %in% valid_colors || grepl("^#[0-9a-fA-F]{3,8}$", s)
    }
    color_fields <- list(
      "Pt Color"    = input$col,
      "Pt Fill"     = input$bg,
      "Line Color"  = input$lincol,
      "Conf. Fill"  = input$conf_fill_col,
      "Conf. Outline" = input$conf_col
    )
    bad_colors <- names(Filter(function(v) !is_valid_color(v), color_fields))
    if (length(bad_colors)) {
      showNotification(
        paste0("Invalid color name(s): ", paste(bad_colors, collapse = ", "),
               ". Please choose from R's built-in colors or use a hex code (#RRGGBB)."),
        type = "error", duration = 8
      )
      return()
    }

    # Weighting flag — depends on which method is active
    weighting_flag <- switch(input$reg_method,
      "LS"     = isTRUE(input$weighting_lin),
      "Deming" = isTRUE(input$weighting_dem),
      FALSE
    )

    # Parse optional axis limits (empty string -> NA)
    to_num <- function(s) {
      v <- suppressWarnings(as.numeric(trimws(s)))
      if (is.na(v)) NA_real_ else v
    }

    # Plot parameters, snapshotted at Graph time
    pp <- list(
      my_pch              = as.integer(input$pointstyle),
      my_point_col        = trimws(input$col),
      my_cex              = as.numeric(input$pointcex),
      my_bg               = trimws(input$bg),
      my_lty              = as.integer(input$lty),
      my_lwd              = as.numeric(input$lwd),
      my_lincol           = trimws(input$lincol),
      xmin                = to_num(input$xmin),
      xmax                = to_num(input$xmax),
      ymin                = to_num(input$ymin),
      ymax                = to_num(input$ymax),
      my_xlab             = input$xlab,
      my_ylab             = input$ylab,
      my_main             = input$main,
      plot_regression     = isTRUE(input$chk_regression),
      plot_method         = isTRUE(input$chk_method),
      plot_rsquared       = isTRUE(input$chk_rsquared),
      plot_identity       = isTRUE(input$chk_identity),
      plot_confidence     = isTRUE(input$chk_confidence),
      plot_conf_outline   = isTRUE(input$chk_conf_outline),
      confidence_fill_col = trimws(input$conf_fill_col),
      confidence_transp   = as.numeric(input$conf_transp),
      conf_col            = trimws(input$conf_col),
      font_mult           = as.integer(input$font_multiplier),
      my_height           = as.numeric(input$img_height),
      my_width            = as.numeric(input$img_width),
      my_dpi              = as.integer(input$img_dpi),
      sdi_lower           = as.numeric(input$sdi_lower),
      sdi_upper           = as.numeric(input$sdi_upper),
      diff_plot_units     = trimws(input$diff_plot_units)
    )

    # All complete points; the analysis uses only those not excluded
    pts  <- data.frame(x = dat$x, y = dat$y, row = seq_len(nrow(dat)),
                       exclude = dat$exclude)
    d    <- pts[!pts$exclude, c("x", "y", "row")]
    excl <- pts$row[pts$exclude]

    res <- withProgress(message = "Running analysis…", value = 0.2, {
      tryCatch({
        set.seed(1)   # same data + settings always give the same bootstrap results
        fit <- switch(input$reg_method,
          "LS"     = fit_ls(d, weighting_flag),
          "PB"     = fit_pb(d, band = pp$plot_confidence),
          "Deming" = fit_deming(d, as.numeric(input$deming_ratio), weighting_flag)
        )
        incProgress(0.6, detail = "Difference plot…")
        ba     <- fit_ba(d, input$diff_style == "abs")
        # Static plots show included points only; the interactive plot's
        # automatic limits also cover excluded points so they can be re-included
        pp_int <- fill_limits(pp, pts)
        pp     <- fill_limits(pp, fit$data)
        list(fit      = fit,
             band     = fit_band(fit, c(pp$xmin, pp$xmax)),
             band_int = fit_band(fit, c(pp_int$xmin, pp_int$xmax)),
             ba       = ba,
             pp       = pp,
             pp_int   = pp_int,
             points   = pts,
             stats    = sub("\n+$", "", paste0(stats_text(fit, excl), ba_stats_text(ba))))
      }, error = function(e) {
        showNotification(paste("Analysis error:", conditionMessage(e)),
                         type = "error", duration = 20)
        NULL
      })
    })
    if (is.null(res)) return()

    rv$result <- res
    rv$pidx   <- 1L
  })

  # ── Plot pages: 1 = regression, 2 = Bland-Altman ─────────────────────────
  PLOT_NAMES <- c("regression", "bland_altman")
  n_pages    <- function() if (is.null(rv$result)) 0L else length(PLOT_NAMES)

  # Draw page `idx` of the current result onto the open device
  draw_page <- function(res, idx) {
    if (idx == 1L) draw_regression(res$fit, res$band, res$pp)
    else           draw_ba(res$ba, res$pp)
  }

  # Write page `idx` to `path` in format `fmt` at the Graph-time image size
  save_page <- function(res, idx, fmt, path) {
    open_device(fmt, path, res$pp)
    on.exit(dev.off())
    draw_page(res, idx)
  }

  observeEvent(input$btn_next, {
    n <- n_pages(); if (n == 0) return()
    rv$pidx <- (rv$pidx %% n) + 1L
  })

  observeEvent(input$btn_prev, {
    n <- n_pages(); if (n == 0) return()
    rv$pidx <- ((rv$pidx - 2L) %% n) + 1L
  })

  output$plot_index_label <- renderText({
    if (n_pages() == 0) return("")
    paste0(rv$pidx, " / ", n_pages())
  })

  # ── Preview image ─────────────────────────────────────────────────────────
  # Same size in inches and pointsize as the download, at screen resolution,
  # so the preview is laid out exactly like the saved file.
  output$plot_preview <- renderUI({
    if (is.null(rv$result))
      return(tags$p(id = "plot_placeholder", "Run the analysis to see plots here."))
    imageOutput("plot_img", height = "auto")
  })

  output$plot_img <- renderImage({
    res <- rv$result
    req(res)
    f <- tempfile(fileext = ".png")
    png(f, width = res$pp$my_width, height = res$pp$my_height, units = "in",
        res = 100, pointsize = res$pp$font_mult)
    draw_page(res, rv$pidx)
    dev.off()
    list(src = f, contentType = "image/png", alt = "Plot preview",
         style = "max-width:100%; max-height:470px;")
  }, deleteFile = TRUE)

  # ── Interactive plotly widget (switches with prev/next) ──────────────────
  # TRUE if the table still holds the points of the last Graph run in the
  # same rows (so plot row numbers still point at the right table rows)
  table_matches_result <- function(df, res) {
    pts <- res$points
    n   <- nrow(pts)
    nrow(df) >= n && identical(df$x[seq_len(n)], pts$x) && identical(df$y[seq_len(n)], pts$y)
  }

  output$interactive_plot <- renderPlotly({
    res <- rv$result
    validate(need(!is.null(res), "Run the analysis to see the interactive plot here."))
    # Show the table's current exclusions (right-clicks since Graph included)
    # without re-rendering on every toggle
    df       <- isolate(rv$df)
    excluded <- if (table_matches_result(df, res)) df$exclude[seq_len(nrow(res$points))]
                else res$points$exclude
    if (rv$pidx == 1L) plotly_regression(res$fit, res$band_int, res$pp_int, res$points, excluded)
    else               plotly_ba(res$ba, res$pp_int, res$points, excluded)
  })

  # ── Right-click exclusion from the interactive plot ──────────────────────
  observeEvent(input$toggle_point, {
    res <- rv$result
    req(res)
    if (!table_matches_result(rv$df, res)) {
      showNotification("The table has changed since the last Graph \u2014 click \u25b6 Graph before excluding points.",
                       type = "warning", duration = 6)
      return()
    }
    row <- as.integer(input$toggle_point$row)
    df  <- rv$df
    df$exclude[row] <- isTRUE(input$toggle_point$excluded)
    rv$df <- df
  })

  # Highlight the Graph button (orange, relabelled "Re-graph") while the table
  # differs from the last run
  observe({
    res <- rv$result
    pending <- FALSE
    if (!is.null(res)) {
      df  <- rv$df
      cur <- df[!is.na(df$x) & !is.na(df$y), c("x", "y", "exclude")]
      rownames(cur) <- NULL
      pending <- !identical(cur, res$points[, c("x", "y", "exclude")])
    }
    shinyjs::toggleClass("btn_graph", "btn-pending", condition = pending)
    updateActionButton(session, "btn_graph",
                       label = if (pending) "▶  Re-graph" else "▶  Graph")
  })

  # ── Stats box ─────────────────────────────────────────────────────────────
  output$stats_box <- renderText({
    if (is.null(rv$result)) "Click '\u25b6 Graph' to run the analysis."
    else rv$result$stats
  })

  # ── Downloads: drawn on demand in the currently selected format ──────────
  current_format <- function() input$plot_type

  output$dl_current_plot <- downloadHandler(
    filename = function() {
      if (n_pages() == 0) return("no_plot.txt")
      paste0(PLOT_NAMES[rv$pidx], ".", current_format())
    },
    content = function(file) {
      req(rv$result)
      save_page(rv$result, rv$pidx, current_format(), file)
    },
    contentType = "application/octet-stream"
  )

  output$dl_all_plots <- downloadHandler(
    filename    = function() {
      paste0("cpR_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content     = function(file) {
      req(rv$result)
      fmt <- current_format()
      dir <- tempfile("cpr_zip_")
      dir.create(dir)
      on.exit(unlink(dir, recursive = TRUE), add = TRUE)
      save_page(rv$result, 1L, fmt, file.path(dir, paste0("plot.",    fmt)))
      save_page(rv$result, 2L, fmt, file.path(dir, paste0("BA_plot.", fmt)))
      writeLines(rv$result$stats, file.path(dir, "stats_output.txt"))
      owd <- setwd(dir)
      on.exit(setwd(owd), add = TRUE, after = FALSE)
      zip::zip(file, list.files())
    },
    contentType = "application/zip"
  )
}

# ── 5. Launch ─────────────────────────────────────────────────────────────────
shinyApp(ui, server)
