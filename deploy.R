# Run from the project root to deploy both apps to Posit Connect.
# Temporarily copies the shared app/R/ files into each app directory
# so the bundle is self-contained, then removes them after deployment.

deploy_app <- function(app_name, app_dir) {
  r_target <- file.path(app_dir, "R")
  dir.create(r_target, showWarnings = FALSE)
  file.copy(list.files("app/R", full.names = TRUE), r_target, overwrite = TRUE)
  on.exit(unlink(r_target, recursive = TRUE), add = TRUE)

  rsconnect::deployApp(
    appDir  = app_dir,
    appName = app_name
  )
}

deploy_app("citeguard-reporter", "app/reporter")
