#    cp-R Shiny: A Shiny Web Interface to R for Clinical Chemists
#    Migrated from the PyQt4 original by Daniel T. Holmes, MD
#    GNU GPL v3 — see gpl-3.0_license.pdf
#
#    "Buy the truth but never sell it" Proverbs 23:23

# ── 1. Install / load R packages ─────────────────────────────────────────────
local({
  needed  <- c("shiny", "rhandsontable", "shinyjs", "jsonlite", "base64enc")
  missing <- needed[!(needed %in% installed.packages()[, "Package"])]
  if (length(missing))
    install.packages(missing, repos = "https://cloud.r-project.org")
})

library(shiny)
library(rhandsontable)
library(shinyjs)
library(jsonlite)
library(base64enc)

# ── 2. Locate the Rdata scripts directory ────────────────────────────────────
# Works whether launched via runApp(), shiny::runApp(), or Rscript app.R
app_path <- tryCatch(
  normalizePath(dirname(sys.frame(1)$ofile)),
  error = function(e) getwd()
)
rdata_dir <- file.path(app_path, "Rdata")

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
      }
      .col-left  .panel-inner { border: 1px solid rgba(92,157,192,.35); }
      .col-mid   .panel-inner { border: 1px solid rgba(160,127,214,.35); }
      .col-right .panel-inner { border: 1px solid rgba(136,136,160,.35); }

      /* ── Sub-section dividers ───────────────────────────────────────────── */
      hr.mini { margin: 7px 0; border: none; border-top: 1px dashed rgba(0,0,0,.12); }

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

      /* ── Confidence band sub-panel ─────────────────────────────────────── */
      .conf-sub {
        background: rgba(255,255,255,.5);
        border: 1px solid rgba(160,127,214,.5);
        border-radius: 5px;
        padding: 7px 8px;
        margin-top: 5px;
      }

      /* ── Color picker widgets ───────────────────────────────────────────── */
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
      var R_COLORS = %s;

      // Build one shared <datalist> and attach to every color input
      document.addEventListener('DOMContentLoaded', function() {
        var dl = document.createElement('datalist');
        dl.id  = 'r-color-list';
        R_COLORS.forEach(function(c) {
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
        // Shiny wraps the input inside a div#id; the actual <input> is inside it
        var wrap = document.getElementById(id);
        if (!wrap) { setTimeout(function(){ attachColor(id); }, 200); return; }
        var inp = wrap.querySelector('input.form-control');
        if (!inp)  { setTimeout(function(){ attachColor(id); }, 200); return; }

        // Link datalist
        inp.setAttribute('list', 'r-color-list');
        inp.setAttribute('autocomplete', 'off');

        // Inject swatch span right after the input
        var swatch = document.createElement('span');
        swatch.className = 'color-swatch';
        swatch.id = 'swatch-' + id;
        inp.parentNode.style.display = 'flex';
        inp.parentNode.style.alignItems = 'center';
        inp.parentNode.style.gap = '4px';
        inp.after(swatch);

        // Live update
        inp.addEventListener('input',  function(){ updateSwatch(id, inp.value); });
        inp.addEventListener('change', function(){ updateSwatch(id, inp.value); });
        updateSwatch(id, inp.value);
      }

      function updateSwatch(id, val) {
        var swatch = document.getElementById('swatch-' + id);
        if (!swatch) return;
        var wrap = document.getElementById(id);
        var valid = R_COLORS.indexOf(val.trim().toLowerCase()) >= 0 ||
                    /^#[0-9a-fA-F]{3,8}$/.test(val.trim());
        if (valid) {
          swatch.style.background = val.trim();
          if (wrap) wrap.classList.remove('color-invalid');
        } else {
          swatch.style.background = '';
          if (wrap) {
            if (val.trim().length > 0) wrap.classList.add('color-invalid');
            else                       wrap.classList.remove('color-invalid');
          }
        }
      }

      // Called from server when a new plot run starts (re-attach if Shiny
      // re-rendered a conditionalPanel and lost the datalist link)
      Shiny.addCustomMessageHandler('reattach_colors', function(ids) {
        ids.forEach(function(id) { attachColor(id); });
      });
    ", jsonlite::toJSON(tolower(colors())))))
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
    # LEFT COLUMN: rhandsontable + data-manipulation buttons
    # ────────────────────────────────────────────────────────────────────────
    column(3,
      div(class = "col-card col-left",
        h5("Data Entry  (x / y)"),
        div(class = "hot-wrap",
          rHandsontableOutput("hot_table", height = "490px")
        ),
        br(),
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
        )
      )
    ),

    # ────────────────────────────────────────────────────────────────────────
    # MIDDLE COLUMN: all graphical / analysis parameters
    # ────────────────────────────────────────────────────────────────────────
    column(3,
      div(class = "col-card col-mid compact",

        # ── Point & Line style ────────────────────────────────────────────
        div(class = "panel-inner",
          h5("Point & Line Style"),
          fluidRow(
            column(6, numericInput("pointstyle", "Pt Style (pch)", 21, 1, 25, 1)),
            column(6, numericInput("pointcex",   "Pt Magnif.",      1, 0.1, 5, 0.1))
          ),
          fluidRow(
            column(6, textInput("col", "Pt Color", "blue")),
            column(6, textInput("bg",  "Pt Fill",  "gray"))
          ),
          fluidRow(
            column(6, numericInput("lty",    "Line Style (lty)", 1, 1, 6, 1)),
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
          uiOutput("interactive_plot")
        ),

        div(class = "panel-inner",
          h5("Statistical Output"),
          div(class = "stats-box",
            verbatimTextOutput("stats_box")
          )
        )

      )
    )

  )
)


# ── 4. Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  INIT_ROWS <- 50

  # Stable tmpdir for this server process
  # Normalise to forward slashes so paths work on all platforms including Windows
  tmpdir <- gsub("\\\\", "/", file.path(tempdir(), paste0("cprshiny_", Sys.getpid())))

  rv <- reactiveValues(
    df       = data.frame(x = rep(NA_real_, INIT_ROWS),
                          y = rep(NA_real_, INIT_ROWS)),
    previews = character(0),
    pidx     = 1L,
    stats    = "Click '\u25b6 Graph' to run the analysis.",
    plot_run = 0L,
    widget   = list()
  )

  # ── rhandsontable ─────────────────────────────────────────────────────────
  output$hot_table <- renderRHandsontable({
    rhandsontable(
      rv$df,
      rowHeaders  = seq_len(nrow(rv$df)),
      colHeaders  = c("x", "y"),
      stretchH    = "all",
      height      = 488,
      contextMenu = TRUE,
      overflow    = "hidden"
    ) %>%
      hot_col("x", type = "numeric", format = "0.[000]") %>%
      hot_col("y", type = "numeric", format = "0.[000]")
  })

  # Sync table edits back to rv$df
  observe({
    req(input$hot_table)
    rv$df <- hot_to_r(input$hot_table)
  })

  # ── Clear ─────────────────────────────────────────────────────────────────
  observeEvent(input$btn_clear, {
    rv$df <- data.frame(x = rep(NA_real_, INIT_ROWS),
                        y = rep(NA_real_, INIT_ROWS))
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

    pad <- max(0L, INIT_ROWS - nrow(new_df))
    if (pad > 0)
      new_df <- rbind(new_df,
                      data.frame(x = rep(NA_real_, pad),
                                 y = rep(NA_real_, pad)))
    rv$df <- new_df
  })

  # ── Sort / Swap ───────────────────────────────────────────────────────────
  get_clean <- function() {
    d <- rv$df
    d[!(is.na(d$x) & is.na(d$y)), ]
  }

  pad_df <- function(d) {
    pad <- max(0L, INIT_ROWS - nrow(d))
    if (pad > 0)
      d <- rbind(d, data.frame(x = rep(NA_real_, pad), y = rep(NA_real_, pad)))
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
    if (!is.null(input$hot_table)) rv$df <- hot_to_r(input$hot_table)

    # Remove any row where EITHER x or y is NA so that the row numbers
    # displayed in the handsontable exactly match the Row: N tooltip in plotly.
    complete_mask <- !is.na(rv$df$x) & !is.na(rv$df$y)
    if (any(!complete_mask)) {
      rv$df <- rv$df[complete_mask, , drop = FALSE]
      rownames(rv$df) <- NULL   # reset to 1, 2, 3, ... to match plotly
    }

    # Create / clean temp dirs
    for (sub in c("plots", "previews", "Rdata")) {
      d <- file.path(tmpdir, sub)
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      old <- list.files(d, full.names = TRUE)
      if (length(old)) file.remove(old)
    }

    # Validate — rv$df is already stripped of incomplete rows above
    dat <- rv$df
    if (nrow(dat) < 3) {
      showNotification("Please enter at least 3 complete (x, y) pairs.", type = "warning")
      return()
    }

    # ── Validate color inputs server-side before sending to R ──────────────
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

    # Build parameter data.frame — column names must EXACTLY match what the
    # R scripts expect when they call read.csv() and optionally attach()
    pp <- data.frame(
      my_pch              = as.integer(input$pointstyle),
      my_point_col        = input$col,
      my_cex              = as.numeric(input$pointcex),
      my_bg               = input$bg,
      my_lty              = as.integer(input$lty),
      my_lwd              = as.numeric(input$lwd),
      my_lincol           = input$lincol,
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
      plot_type           = input$plot_type,
      plot_confidence     = isTRUE(input$chk_confidence),
      delta               = as.numeric(input$deming_ratio),
      plot_difference_abs = (input$diff_style == "abs"),
      confidence_fill_col = input$conf_fill_col,
      confidence_transp   = as.numeric(input$conf_transp),
      my_height           = as.numeric(input$img_height),
      my_width            = as.numeric(input$img_width),
      my_dpi              = as.integer(input$img_dpi),
      font_mult           = as.integer(input$font_multiplier),
      plot_conf_outline   = isTRUE(input$chk_conf_outline),
      conf_col            = input$conf_col,
      weighting           = weighting_flag,
      sdi_lower           = as.numeric(input$sdi_lower),
      sdi_upper           = as.numeric(input$sdi_upper),
      diff_plot_units     = ifelse(nzchar(trimws(input$diff_plot_units)),
                                  input$diff_plot_units, NA_character_),
      reg_method          = input$reg_method,
      stringsAsFactors    = FALSE
    )

    # Write parameter CSV (tab-separated, quoted strings)
    write.table(
      pp,
      file      = file.path(tmpdir, "Rdata", "plot_parameters.csv"),
      sep       = "\t",
      quote     = TRUE,
      row.names = FALSE,
      na        = "NA"
    )

    # Write regression data CSV (no header, tab-separated, NA for missing)
    write.table(
      dat,
      file      = file.path(tmpdir, "Rdata", "regression_data.csv"),
      sep       = "\t",
      quote     = FALSE,
      row.names = FALSE,
      col.names = FALSE,
      na        = "NA"
    )

    # Run scripts via Rscript subprocess — isolated environment, works on
    # local and server deployment. R.home("bin") finds Rscript without
    # relying on it being on the system PATH.
    rscript_bin <- file.path(R.home("bin"), "Rscript")

    run_r <- function(script_name, log_name = "Rscript.log") {
      rscript  <- file.path(rdata_dir, script_name)
      log_file <- file.path(tmpdir, "Rdata", log_name)
      safe_tmp <- gsub("\\\\", "/", tmpdir)
      cmd <- sprintf(
        '"%s" --vanilla "%s" "tmpdir=\'%s\'" > "%s" 2>&1',
        rscript_bin, rscript, safe_tmp, log_file
      )
      ret <- system(cmd, intern = FALSE, ignore.stderr = FALSE)
      if (ret != 0) {
        txt <- if (file.exists(log_file))
          tail(readLines(log_file, warn = FALSE), 30)
        else "(no log)"
        showNotification(
          paste0(script_name, " error:\n", paste(txt, collapse = "\n")),
          type = "error", duration = 20
        )
      }
      invisible(ret)
    }

    withProgress(message = "Running R analysis\u2026", value = 0.05, {
      incProgress(0.25, detail = "Regression\u2026")
      switch(input$reg_method,
        "LS"     = run_r("linear_regression.R", "regression.log"),
        "PB"     = run_r("PB_regression.R",     "regression.log"),
        "Deming" = run_r("Deming_regression.R", "regression.log")
      )
      incProgress(0.45, detail = "Difference plot\u2026")
      run_r("BA_plot.R", "BA.log")
      setProgress(1.0, detail = "Done.")
    })

    # Load previews
    rv$previews <- sort(list.files(
      file.path(tmpdir, "previews"),
      pattern    = "\\.jpg$",
      full.names = TRUE
    ))
    rv$pidx     <- 1L
    rv$plot_run <- rv$plot_run + 1L   # invalidates renderUI regardless of filenames

    # Load widget paths (regression.html and bland_altman.html)
    widget_dir  <- file.path(tmpdir, "widgets")
    rv$widget   <- list(
      regression   = file.path(widget_dir, "regression.html"),
      bland_altman = file.path(widget_dir, "bland_altman.html")
    )

    # Re-attach colour autocomplete datalists after conditionalPanel re-renders
    session$sendCustomMessage("reattach_colors",
                              list("col", "bg", "lincol", "conf_fill_col", "conf_col"))

    # Load stats text
    stats_path <- file.path(tmpdir, "plots", "stats_output.txt")
    rv$stats <- if (file.exists(stats_path)) {
      paste(readLines(stats_path, warn = FALSE), collapse = "\n")
    } else {
      "(stats_output.txt not found \u2014 check R log for errors)"
    }
  })

  # ── Plot navigation ───────────────────────────────────────────────────────
  observeEvent(input$btn_next, {
    n <- length(rv$previews); if (n == 0) return()
    rv$pidx <- (rv$pidx %% n) + 1L
  })

  observeEvent(input$btn_prev, {
    n <- length(rv$previews); if (n == 0) return()
    rv$pidx <- ((rv$pidx - 2L) %% n) + 1L
  })

  output$plot_index_label <- renderText({
    if (length(rv$previews) == 0) return("")
    paste0(rv$pidx, " / ", length(rv$previews))
  })

  # ── Preview image (base64 inline) ─────────────────────────────────────────
  output$plot_preview <- renderUI({
    # Explicitly take a dependency on plot_run so this re-executes on every
    # new graph run, even when filenames on disk are identical to last time.
    rv$plot_run
    if (length(rv$previews) == 0)
      return(tags$p(id = "plot_placeholder", "Run the analysis to see plots here."))
    f   <- rv$previews[rv$pidx]
    b64 <- base64encode(f)
    tags$img(
      src   = paste0("data:image/jpeg;base64,", b64),
      style = "max-width:100%; max-height:470px;"
    )
  })

  # ── Interactive plotly widget (iframe, switches with prev/next) ──────────
  output$interactive_plot <- renderUI({
    rv$plot_run   # re-render on every new graph run
    w <- rv$widget
    if (length(w) == 0 || !file.exists(w$regression))
      return(tags$p(style = "color:#8888a0; padding:20px; text-align:center;",
                    "Run the analysis to see the interactive plot here."))

    # Decide which widget to show based on the current preview
    stem <- if (length(rv$previews) > 0)
      tools::file_path_sans_ext(basename(rv$previews[rv$pidx]))
    else "A"

    html_path <- if (stem == "B" && file.exists(w$bland_altman)) {
      w$bland_altman
    } else {
      w$regression
    }

    # Register a resource path keyed to the run so Shiny serves the file
    alias <- paste0("cprwidget_", rv$plot_run)
    addResourcePath(alias, dirname(html_path))
    src   <- paste0(alias, "/", basename(html_path))

    tags$iframe(
      src             = src,
      style           = "width:100%; height:480px; border:none;",
      scrolling       = "no",
      frameborder     = "0",
      allowtransparency = "true"
    )
  })

  # ── Stats box ─────────────────────────────────────────────────────────────
  output$stats_box <- renderText({
    rv$plot_run   # take dependency so it always refreshes with the plot
    rv$stats
  })

  # ── Download current preview ──────────────────────────────────────────────
  # Map preview filename stem → output base name
  preview_to_name <- c(
    "A" = "regression",
    "B" = "bland_altman"
  )

  output$dl_current_plot <- downloadHandler(
    filename = function() {
      if (length(rv$previews) == 0) return("no_plot.txt")
      stem <- tools::file_path_sans_ext(basename(rv$previews[rv$pidx]))
      nm   <- preview_to_name[[stem]]
      if (is.null(nm)) nm <- stem
      fmt  <- isolate(input$plot_type)
      if (is.null(fmt) || !nzchar(fmt)) fmt <- "jpg"
      paste0(nm, ".", fmt)
    },
    content = function(file) {
      req(length(rv$previews) > 0)
      stem <- tools::file_path_sans_ext(basename(rv$previews[rv$pidx]))
      nm   <- preview_to_name[[stem]]
      if (is.null(nm)) nm <- stem
      fmt  <- isolate(input$plot_type)
      if (is.null(fmt) || !nzchar(fmt)) fmt <- "jpg"
      # Regression plot is saved as "plot.*"; BA plot as "BA_plot.*"
      src_stem <- switch(nm,
        "regression"  = "plot",
        "bland_altman" = "BA_plot",
        nm
      )
      src <- file.path(tmpdir, "plots", paste0(src_stem, ".", fmt))
      if (file.exists(src)) {
        file.copy(src, file)
      } else {
        showNotification(
          paste0("File not found: ", basename(src), " — please re-run the analysis."),
          type = "warning", duration = 6
        )
      }
    },
    contentType = "application/octet-stream"
  )

  # ── Download all static outputs as zip ───────────────────────────────────
  output$dl_all_plots <- downloadHandler(
    filename    = function() {
      paste0("cpR_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content     = function(file) {
      all_files <- list.files(file.path(tmpdir, "plots"), full.names = TRUE)
      req(length(all_files) > 0)
      owd <- setwd(file.path(tmpdir, "plots"))
      on.exit(setwd(owd), add = TRUE)
      zip(file, basename(all_files))
    },
    contentType = "application/zip"
  )
}

# ── 5. Launch ─────────────────────────────────────────────────────────────────
shinyApp(ui, server)
