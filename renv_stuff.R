### Title:    Do renv Admin Stuff for the jaspMissingData Module
### Author:   Kyle M. Lang
### Created:  2024-11-05
### Modified: 2024-11-12

rm(list = ls(all = TRUE))

renv::deactivate(clean = TRUE)

# update.packages()

renv::status()
?renv::status

renv::rehash()

# renv::install(
#   c("car",
#     "combinat",
#     "ggmice",
#     "ggplot2",
#     "lmtest",
#     "matrixStats",
#     "mice",
#     "testthat")
# )

# renv::install("jasp-stats/jaspBase")
# renv::install("jasp-stats/jaspGraphs")
# renv::install("jasp-stats/jaspTools")
# renv::install("jasp-stats/jaspRegression")

# renv::activate()
renv::init()
renv::restore()
renv::install('.')
renv::snapshot()

.libPaths()
