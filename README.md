# cp-R Shiny

A Shiny web-application port of **cp-R 0.4** (Chemical Pathology R),
originally a PyQt4 desktop application by Dr. Daniel T. Holmes, MD.

## Features

- **Least Squares**, **Passing-Bablok**, and **Deming** regression methods
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
├── Dockerfile            <- Docker build definition
└── README.md
```

All analysis runs inside the Shiny process: packages load once at startup,
results are kept in memory, and download files are drawn only when requested.
Bootstraps use a fixed seed, so identical inputs give identical results, and
run across CPU cores where the platform supports forking (Linux, macOS).

## License

GNU GPL v3 — see `gpl-3.0_license.pdf` in the original cp-R distribution.
