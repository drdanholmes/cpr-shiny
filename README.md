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

### Shiny app (`app.R`)
| Package | Purpose |
|---|---|
| `shiny` | Web framework |
| `rhandsontable` | Interactive data-entry grid |
| `shinyjs` | JS helpers |
| `jsonlite` | Colour list serialisation |
| `base64enc` | Static plot preview encoding |

### R scripts (`Rdata/`)
| Package | Purpose |
|---|---|
| `ggplot2` | Plot construction |
| `plotly` | Interactive widget rendering |
| `htmlwidgets` | Self-contained HTML widget export |
| `boot` | Bootstrap CIs (Passing-Bablok, Deming) |
| `compiler` | JIT compilation of PB inner loop (base R — no install needed) |

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
├── app.R                     <- Shiny UI + server
├── Dockerfile                <- Docker build definition
├── README.md
└── Rdata/
    ├── linear_regression.R   <- Least Squares regression
    ├── PB_regression.R       <- Passing-Bablok regression
    ├── Deming_regression.R   <- Deming regression
    └── BA_plot.R             <- Bland-Altman difference plot
```

Each R script is invoked as an isolated `Rscript` subprocess — environment
variables from the Shiny session are not inherited, which ensures reproducible
behaviour on multi-user servers.

## License

GNU GPL v3 — see `gpl-3.0_license.pdf` in the original cp-R distribution.
