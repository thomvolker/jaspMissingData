#
# Copyright (C) 2024 Utrecht University
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

###-Main Function----------------------------------------------------------------------------------------------------###

#' Multiply impute missing data with MICE
#' @export
MissingDataImputation <- function(jaspResults, dataset, options) {

  saveRDS(dataset, "~/software/jasp/modules/imputation/data/dataset.rds")

  # Set title
  jaspResults$title <- "Multiple Imputation with MICE"

  # Init options: add variables to options to be used in the remainder of the analysis
  options <- .initImputationOptions(options)

  # ready <- length(options$variables) > 0
  if (.readyForMi(options)) {
    # read dataset
    # dataset <- .readData(options)
    # error checking
    errors <- .errorHandling(dataset, options)

    # Output containers, tables, and plots based on the results. These functions should not return anything!
    # .createImputationContainer(jaspResults, options)

    # browser() #########################################################################################################

    .initMiceMids(jaspResults)
    options <- .imputeMissingData(jaspResults[["MiceMids"]], options, dataset)

    ## Initialize containers to hold the convergence plots and analysis results:
    # convergencePlots  <- .initConvergencePlots(jaspResults)
    # analysisContainer <- .initAnalysisContainer(jaspResults)
    .initConvergencePlots(jaspResults)
    .initAnalysisContainer(jaspResults)

    if (options$tracePlot)
      .createTracePlot(jaspResults[["ConvergencePlots"]], jaspResults[["MiceMids"]])
    if (options$densityPlot)
      .createDensityPlot(jaspResults[["ConvergencePlots"]], jaspResults[["MiceMids"]], options)

    if (options$runLinearRegression) {
      .lmFunction <<- .linregSetFittingFunction(options) # The deep assignment here is almost certainly a stupid idea

      .runRegression(jaspResults, jaspResults[["MiceMids"]], options)

      # miFits <- .estimateRegressionModels(jaspResults[["MiceMids"]], options)
      # saveRDS(miFits, "/home/kylelang/software/jasp/modules/imputation/data/miFits.rds")
      # modelContainer <- .poolRegressionEstimates(jaspResults, miFits, options, offset = 0)
      # .populateRegressionResults(jaspResults, modelContainer, options)
      # .runRegression(jaspResults[["AnalysisContainer"]], jaspResults[["MiceMids"]], options)
    }
  }

  return()
}

###------------------------------------------------------------------------------------------------------------------###

# .createImputationContainer <- function(jaspResults, options) {
#   if (!is.null(jaspResults[["ImputationContainer"]])) return()
#   
#   imputationContainer <- createJaspContainer("Missing Data Imputation")
#   imputationContainer$dependOn(options = c("variables", "groupVar", "nImps", "nIter", "seed"))
#   
#   jaspResults[["ImputationContainer"]] <- imputationContainer
# }

###-Init Functions---------------------------------------------------------------------------------------------------###

.initImputationOptions <- function(options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis
  options$lastMidsUpdate <- Sys.time()
  options$imputedVariables <- ""
  options$fType <- 1
  options$lmFunction <- pooledLm

  options
}

###------------------------------------------------------------------------------------------------------------------###

.initMiceMids <- function(jaspResults) {
  if(!is.null(jaspResults[["MiceMids"]])) return()

  miceMids <- createJaspState()
  miceMids$dependOn(options = c("variables", "nImps", "nIter", "seed"))

  jaspResults[["MiceMids"]] <- miceMids
}

###------------------------------------------------------------------------------------------------------------------###

.initConvergencePlots <- function(jaspResults) {
  if(!is.null(jaspResults[["ConvergencePlots"]])) return()

  convergencePlots <- createJaspContainer(title = "Convergence Plots")
  convergencePlots$dependOn(options = "lastMidsUpdate")

  jaspResults[["ConvergencePlots"]] <- convergencePlots
  
  # jaspResults[["ConvergencePlots"]]
}

###------------------------------------------------------------------------------------------------------------------###

.initAnalysisContainer <- function(jaspResults) {
  if(!is.null(jaspResults[["AnalysisContainer"]])) return()
  
  analysisContainer <- createJaspContainer(title = "Analyses")
  analysisContainer$dependOn(options = "lastMidsUpdate")

  jaspResults[["AnalysisContainer"]] <- analysisContainer
  
  # jaspResults[["AnalysisContainer"]]
}

###-Output Functions-------------------------------------------------------------------------------------------------###

.imputeMissingData <- function(miceMids, options, dataset) {

  miceOut <- try(
    with(options,
      mice::mice(
        # data  = dataset[ , encodeColNames(variables), drop = FALSE],
        # data  = dataset[variables],
        data  = dataset,
        m     = nImp,
        maxit = nIter,
        seed  = seed,
        print = FALSE
      )
    )
  )

  saveRDS(dataset, "~/software/jasp/modules/imputation/data/dataset2.rds")
  saveRDS(miceOut, "~/software/jasp/modules/imputation/data/miceOut.rds")
  saveRDS(options$variables, "~/software/jasp/modules/imputation/data/variables.rds")

  if (!inherits(miceOut, "try-error")) {
    # saveRDS(miceOut, "/home/kylelang/software/jasp/modules/imputation/data/miceOut.rds")
    miceMids$object <- miceOut
    options$lastMidsUpdate <- Sys.time()
    options$imputedVariables <- (sapply(miceOut$imp, nrow) > 0) |> which() |> names()
  } else {
    stop(
      "The mice() function crashed when attempting to impute the missing data.\n",
      "The error message returned by mice is shown below.\n",
      miceOut
    )
  }

  options
}

###------------------------------------------------------------------------------------------------------------------###

.createTracePlot <- function(convergencePlots, miceMids) {
  # if (!is.null(jaspResults[["tracePlot"]])) return()

  tracePlot <- createJaspPlot(title = "Trace Plot", height = 320, width = 480)
  # tracePlot$dependOn(options = "variables")
  
  # Bind plot to jaspResults via the convergencePlots container:
  convergencePlots[["TracePlot"]] <- tracePlot

  tracePlot$plotObject <- miceMids$object |> ggmice::plot_trace()
}

###------------------------------------------------------------------------------------------------------------------###

.createDensityPlot <- function(convergencePlots, miceMids, options) {

  convergencePlots[["DensityPlots"]] <- createJaspContainer("Density Plots")

  for(v in options$imputedVariables) {
    densityPlot <- createJaspPlot(title = v, height = 320, width = 480)
    # densityPlot$dependOn(options = "variables")

    ## Bind the density plot for variable 'v' to the 'densityPlots' container in jaspResults
    convergencePlots[["DensityPlots"]][[v]] <- densityPlot

    ## Populate the plot object
    densityPlot$plotObject <-
      ggmice::ggmice(miceMids$object, ggplot2::aes(x = .data[[v]], group = .imp)) +
      ggplot2::geom_density()
  }
}

###------------------------------------------------------------------------------------------------------------------###
