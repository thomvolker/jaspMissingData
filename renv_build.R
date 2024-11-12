### Title:    Build package with renv
### Author:   Kyle M. Lang
### Created:  2024-11-05
### Modified: 2024-11-12

## Let renv do things without asking:
renv::consent(provided = TRUE)

if (is.null(renv::project())) {
  message("This isn't an active renv project.")
  renv::activate()
} else {
  message("We're in an active renv project.")
}

message("Restoring/synchronizing the project library.")
renv::restore(clean = TRUE)

message("Installing the package.")
renv::install('.')

message("R libPath for developer mode:\n", .libPaths()[1])
