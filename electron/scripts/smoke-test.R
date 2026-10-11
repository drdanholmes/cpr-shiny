# Check the app's code and packages load in the bundled R (run from electron/).
.libPaths(.Library)
setwd("..")
shiny::loadSupport()
app <- source("app.R")$value
stopifnot(inherits(app, "shiny.appobj"))
cat("App loaded OK\n")
