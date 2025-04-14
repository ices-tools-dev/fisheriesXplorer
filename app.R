# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
#fisheriesXplorer::run_app() # add parameters here (if any)
run_this_app <- get("run_app", envir = as.environment("package:fisheriesXplorer"))
run_this_app() # add parameters here (if any)