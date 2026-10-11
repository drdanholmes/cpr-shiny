# Install the app's packages into the bundled R's own library.
# Run with the bundled Rscript: r-win/bin/Rscript.exe --vanilla scripts/install-pkgs.R

pkgs <- c("shiny", "rhandsontable", "shinyjs", "jsonlite", "htmlwidgets",
          "plotly", "boot", "mcr", "zip")

.libPaths(.Library)
options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages(pkgs, lib = .Library, type = "binary")

# install.packages() only warns on failure, so check explicitly.
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Failed to install: ", paste(missing, collapse = ", "))
}

# Drop per-package docs and help to shrink the bundle.
for (p in list.files(.Library, full.names = TRUE)) {
  unlink(file.path(p, c("doc", "html", "help")), recursive = TRUE)
}

cat("Installed into", .Library, "\n")
print(installed.packages(lib.loc = .Library)[pkgs, "Version"])
