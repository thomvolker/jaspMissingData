### Title:    Do renv Admin Stuff for the jaspImputation Module
### Author:   Kyle M. Lang
### Created:  2024-11-05
### Modified: 2024-11-05

rm(list = ls(all = TRUE))

# renv::deactivate(clean = TRUE)
# renv::activate()

update.packages()

renv::status()

renv::install(
  c("car",
    "combinat",
    "ggmice",
    "ggplot2",
    "lmtest",
    "matrixStats",
    "mice",
    "testthat")
)

renv::install("jasp-stats/jaspBase")
renv::install("jasp-stats/jaspGraphs")
renv::install("jasp-stats/jaspTools")
renv::install("jasp-stats/jaspRegression")

renv::snapshot()

renv::activate()
renv::restore()

renv::install('.')

.libPaths()

