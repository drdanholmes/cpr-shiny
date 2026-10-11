# cp-R Shiny

A Shiny web-application port of **cp-R 0.4** (Chemical Pathology R),
originally a PyQt4 desktop application by Dr. Daniel T. Holmes, MD.

## Features

- **Least Squares** (optionally 1/x² weighted), **Passing-Bablok**, and **Deming**
  (optionally weighted, constant CV) regression methods
- Right-click a point in the interactive plot to exclude it from (or re-include it in) the analysis
- **Bland-Altman** difference plot
- Interactive **plotly** widgets with per-point tooltips showing data-table row numbers
- Static image downloads in JPG, PNG, TIFF, BMP, PDF, and PS formats
- Fully interactive data entry via `rhandsontable` (paste from spreadsheet, sort, insert/delete rows)

## R Package Dependencies

| Package | Purpose |
|---|---|
| `shiny` | Web framework |
| `rhandsontable` | Interactive data-entry grid |
| `shinyjs` | JS helpers |
| `jsonlite` | Colour list serialisation |
| `plotly` | Interactive plots |
| `mcr` | Passing-Bablok estimates and analytical CIs |
| `boot` | Bootstrap CIs and confidence bands (Passing-Bablok, Deming) |

All packages are auto-installed on first run if missing.

## Running locally

```r
# From R or RStudio:
shiny::runApp("path/to/cpr-shiny")
```

```bash
# From the terminal:
Rscript -e "shiny::runApp('cpr-shiny')"
```

## Docker deployment

### Build
```bash
docker build -t cpr-shiny .
```

### Run
```bash
docker run --rm -p 3838:3838 cpr-shiny
```

Then open **http://localhost:3838** in your browser.

### Run with persistent logs
```bash
docker run --rm -p 3838:3838 \
  -v /path/to/logs:/var/log/shiny-server \
  cpr-shiny
```

### Docker Compose
```yaml
services:
  cpr-shiny:
    build: .
    ports:
      - "3838:3838"
    restart: unless-stopped
```

## Cloud deployment (shinyapps.io)

```r
library(rsconnect)
rsconnect::deployApp("path/to/cpr-shiny")
```

## Directory layout

```
cpr-shiny/
├── app.R                 <- Shiny UI + server
├── R/                    <- sourced automatically by Shiny at startup
│   ├── analysis.R        <- LS / Passing-Bablok / Deming / Bland-Altman calculations and stats text
│   ├── plots_base.R      <- static plots (preview and downloads)
│   └── plots_plotly.R    <- interactive plots
├── tests/
│   └── compare_mcr.R     <- regression results checked against mcr
├── Dockerfile            <- Docker build definition
└── README.md
```

All analysis runs inside the Shiny process: packages load once at startup,
results are kept in memory, and download files are drawn only when requested.
Bootstraps use a fixed seed, so identical inputs give identical results, and
run across CPU cores where the platform supports forking (Linux, macOS).

## Validation against mcr

`tests/compare_mcr.R` checks the app's regression functions in `R/analysis.R`
against `mcr::mcreg` (tested with mcr 1.3.3.1). It uses `test_data.csv` and synthetic
datasets with constant error, proportional error, integer ties and a negative slope,
plus Deming error ratios of 1, 0.25 and 4. Run it from the project root:

```bash
Rscript tests/compare_mcr.R
```

It exits non-zero if any check differs by more than 1e-6.

| Method | mcr equivalent | Result |
|---|---|---|
| Least Squares: estimates and 95% CIs | `LinReg` | Identical |
| Weighted Least Squares (1/x²): estimates and slope CI | `WLinReg` | Identical |
| Passing-Bablok: estimates and analytical CIs | `PaBa` | Identical (the app calls `mcreg` directly) |
| Passing-Bablok bootstrap estimator for the confidence band (`pb_coef`) | `PaBa`, `slope.measure = "tangent"` | Identical |
| Deming, unweighted and weighted, all error ratios | `Deming`, `WDeming` | Identical (to the convergence tolerance) |
| Deming bootstrap BCa CIs | `Deming`, `method.ci = "bootstrap"` | Close; not exact because both are bootstraps |

**Error-ratio convention:** the app's Deming input is Var(y)/Var(x). mcr's
`error.ratio` is the inverse, Var(x)/Var(y), so an app value of 4 corresponds to
`error.ratio = 0.25`.

**Limitations inherited from mcr:** mcr refuses Passing-Bablok fits on data
containing negative values. For those data the app reports an error rather than a fit.

## License

GNU GPL v3 — see `gpl-3.0_license.pdf` in the original cp-R distribution.
