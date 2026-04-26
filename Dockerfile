# cp-R Shiny — Docker image
# rocker/shiny:4.4.2 is built on Ubuntu 24.04 (Noble Numbat)
FROM rocker/shiny:4.4.2

# ── System libraries ──────────────────────────────────────────────────────────
# libcurl / libssl  : needed by httr / curl (plotly dependency)
# libxml2           : needed by xml2 (htmlwidgets dependency)
# libfontconfig / libfreetype : needed by Cairo (PDF/PS output)
# libharfbuzz / libfribidi   : needed by textshaping (ggplot2 text)
# libtiff / libpng / libjpeg : raster image devices
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libtiff-dev \
      libpng-dev \
      libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# ── R packages ────────────────────────────────────────────────────────────────
# Install all dependencies in one RUN so Docker layer caching is efficient.
# compiler is a base package — no need to install.
RUN Rscript -e "\
  pkgs <- c( \
    'shiny', 'rhandsontable', 'shinyjs', 'jsonlite', 'base64enc', \
    'ggplot2', 'plotly', 'htmlwidgets', 'boot', 'mcr' \
  ); \
  install.packages(pkgs, repos='https://cloud.r-project.org', \
                   Ncpus=parallel::detectCores()); \
"

# ── App ───────────────────────────────────────────────────────────────────────
# rocker/shiny serves apps from /srv/shiny-server/
COPY . /srv/shiny-server/cpr-shiny/

# Shiny Server config — single-app mode, port 3838
RUN echo '\
run_as shiny; \n\
\n\
server { \n\
  listen 3838; \n\
  location / { \n\
    app_dir /srv/shiny-server/cpr-shiny; \n\
    log_dir /var/log/shiny-server; \n\
    directory_index off; \n\
  } \n\
} \n\
' > /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
