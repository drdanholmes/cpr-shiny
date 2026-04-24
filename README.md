# cp-R Shiny

A Shiny web-application port of **cp-R 0.4** (Chemical Pathology R),
originally a PyQt4 desktop app by Dr. Daniel T. Holmes, MD.

## What was migrated

| Original (PyQt4)             | Shiny equivalent                                  |
|------------------------------|---------------------------------------------------|
| `QTableWidget` (1 000 rows)  | `rhandsontable` — fully interactive, sortable     |
| Clipboard paste button       | Modal dialog with text-area paste                 |
| Radio/checkbox controls      | `radioButtons` / `checkboxInput`                  |
| QLabel image preview         | Base64-encoded `<img>` rendered in `uiOutput`     |
| Previous / Next plot buttons | Reactive index cycling through preview list       |
| Plain-text stats box         | `verbatimTextOutput`                              |
| File → Save Images           | `downloadButton` (saves current preview)          |

The **R scripts are kept exactly as-is**:

- `Rdata/linear_regression.R`
- `Rdata/PB_regression.R`
- `Rdata/Deming_regression.R`
- `Rdata/BA_plot.R`

They are invoked via `R CMD BATCH` exactly as the original Python code did.

## Prerequisites

- R ≥ 4.0
- The following CRAN packages (auto-installed on first run):
  `shiny`, `rhandsontable`, `shinyjs`, `base64enc`
- The original R scripts also require: `car`, `calibrate`, `boot`, `compiler`
  (auto-installed by the scripts themselves).

## Running

```r
# From within R or RStudio:
shiny::runApp("path/to/cpr-shiny")
```

Or from the terminal:

```bash
Rscript -e "shiny::runApp('cpr-shiny')"
```

## Directory layout

```
cpr-shiny/
├── app.R          ← Shiny UI + server (this file replaces the PyQt .py)
├── README.md
└── Rdata/
    ├── linear_regression.R   ← unchanged from original
    ├── PB_regression.R       ← unchanged from original
    ├── Deming_regression.R   ← unchanged from original
    └── BA_plot.R             ← unchanged from original
```

## Key feature: rhandsontable

The data-entry grid uses `rhandsontable`, which gives you:

- Click-to-edit individual cells
- Right-click context menu (copy, paste, insert row, delete row)
- Column sorting by clicking a header
- Paste directly from Excel / Numbers / LibreOffice into the grid
  (select the top-left cell and Ctrl+V / Cmd+V)
- The **Paste Data** button also works via a modal dialog for
  systems where direct clipboard access isn't available.

## License

GNU GPL v3 — see `gpl-3.0_license.pdf` in the original distribution.
