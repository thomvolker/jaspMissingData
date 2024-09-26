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

###-Main Function------------------------------------------------------------------------------------------------------------###

#' Multiply impute missing data with MICE
#' @export
ImputationInternal <- function(jaspResults, dataset, options) {
  # Set title
  jaspResults$title <- "Multiple Imputation with MICE"

  # Init options: add variables to options to be used in the remainder of the analysis
  options <- .initImputationOptions(options)

  ready <- length(options$variables) > 0
  if(ready) {
    # read dataset
    dataset <- .readData(options)
    # error checking
    errors <- .errorHandling(dataset, options)

    # Output containers, tables, and plots based on the results. These functions should not return anything!
    # .createImputationContainer(jaspResults, options)

    miceMids <- .initMiceMids()
    options <- .imputeMissingData(miceMids, options, dataset)

    .initConvergencePlots(jaspResults)

    if(options$tracePlot)
      .createTracePlot(jaspResults, miceMids)
    if(options$densityPlot)
      .createDensityPlot(jaspResults, miceMids, options)

    if(options$runLinearRegression)
      .runRegression(jaspResults, miceMids, options)
  }
    return()
}

###-Common Functions (We shouldn't be copying these)-------------------------------------------------------------------------###

.readData <- function(options) {
  vars <- unlist(options$variables)
  # Read in the dataset using the built-in functions
  if (!is.null(options$groupVar) && options$groupVar == "")
    .readDataSetToEnd(columns = vars, columns.as.factor = options$groupVar)
  else
    .readDataSetToEnd(columns = vars)
}

###--------------------------------------------------------------------------------------------------------------------------###

.errorHandling <- function(dataset, options)
  .hasErrors(dataset, 
             "run", 
             type = c('observations', 'variance', 'infinity'),
             all.target = options$variables,
             observations.amount = '< 2',
             exitAnalysisIfErrors = TRUE)

###--------------------------------------------------------------------------------------------------------------------------###

# .createImputationContainer <- function(jaspResults, options) {
#   if (!is.null(jaspResults[["ImputationContainer"]])) return()
#   
#   imputationContainer <- createJaspContainer("Missing Data Imputation")
#   imputationContainer$dependOn(options = c("variables", "groupVar", "nImps", "nIter", "seed"))
#   
#   jaspResults[["ImputationContainer"]] <- imputationContainer
# }

###-Init Functions-----------------------------------------------------------------------------------------------------------###

.initImputationOptions <- function(options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis
  options$lastMidsUpdate <- Sys.time()
  options$imputedVariables <- ""
  options
}

###--------------------------------------------------------------------------------------------------------------------------###

.initMiceMids <- function() {
  miceMids <- createJaspState()
  miceMids$dependOn(options = c("variables", "nImps", "nIter", "seed"))

  miceMids
}

###--------------------------------------------------------------------------------------------------------------------------###

.initConvergencePlots <- function(jaspResults) {
  if(!is.null(jaspResults[["ConvergencePlots"]])) return()

  convergencePlots <- createJaspContainer(title = "Convergence Plots")
  convergencePlots$dependOn(options = "lastMidsUpdate")

  jaspResults[["ConvergencePlots"]] <- convergencePlots
}

###-Output Functions---------------------------------------------------------------------------------------------------------###

#' @importFrom mice mice complete
.imputeMissingData <- function(miceMids, options, dataset) {

  miceOut <- tryCatch(
    with(options,
      mice(data  = dataset[ , encodeColNames(variables), drop = FALSE],
        m     = nImp,
        maxit = nIter,
        seed  = seed,
        print = FALSE)
    )
  )

  if(class(miceOut) != "try-error") {
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

###--------------------------------------------------------------------------------------------------------------------------###

#' @importFrom ggmice plot_trace
.createTracePlot <- function(convergencePlots, miceMids) {
  # if (!is.null(jaspResults[["tracePlot"]])) return()

  tracePlot <- createJaspPlot(title = "Trace Plot", height = 320, width = 480)
  # tracePlot$dependOn(options = "variables")
  
  # Bind plot to jaspResults via the convergencePlots container:
  convergencePlots[["tracePlot"]] <- tracePlot

  tracePlot$plotObject <- miceMids$object |> plot_trace()
}

###--------------------------------------------------------------------------------------------------------------------------###

#' @importFrom ggmice ggmice densityplot
#' @importFrom ggplot2 aes geom_density
.createDensityPlot <- function(convergencePlots, miceMids, options) {

  convergencePlots[["densityPlots"]] <- createJaspContainer("Density Plots")

  for(v in options$imputedVariables) {
    densityPlot <- createJaspPlot(title = v, height = 320, width = 480)
    # densityPlot$dependOn(options = "variables")

    ## Bind the density plot for variable 'v' to the 'densityPlots' container in jaspResults
    convergencePlots[["densityPlots"]][[v]] <- densityPlot

    ## Populate the plot object
    densityPlot$plotObject <- miceMids$object |> ggmice(aes(x = .data[[v]], group = .imp)) + geom_density()
  }
}

###--------------------------------------------------------------------------------------------------------------------------###

.runRegression <- function(jaspResults, miceMids, options, offset = 2) {
  ready <- inherits(miceMids$object, "mids") && # We can't do an analysis before imputing
    options$dependent != "" &&
    (length(unlist(options$modelTerms)) > 0 || options$interceptTerm)

  modelContainer <- jaspRegression:::.linregGetModelContainer(jaspResults, position = offset + 1)

  dataset <- miceMids$object |> complete(1) 
  # dataset <- jaspResults[["miceMids"]]$object |> complete(1) 
  model   <- jaspRegression:::.linregCalcModel(modelContainer, dataset, options, ready)

  if (is.null(modelContainer[["summaryTable"]]))
    jaspRegression:::.linregCreateSummaryTable(modelContainer, model, options, position = 1)

  if (options$modelFit && is.null(modelContainer[["anovaTable"]]))
    jaspRegression:::.linregCreateAnovaTable(modelContainer, model, options, position = 2)

  if (options$coefficientEstimate && is.null(modelContainer[["coeffTable"]]))
    jaspRegression:::.linregCreateCoefficientsTable(modelContainer, model, dataset, options, position = 3)

  if (options$coefficientBootstrap && is.null(modelContainer[["bootstrapCoeffTable"]]))
    jaspRegression:::.linregCreateBootstrapCoefficientsTable(modelContainer, model, dataset, options, position = 4)

  if (options$partAndPartialCorrelation && is.null(modelContainer[["partialCorTable"]]))
    jaspRegression:::.linregCreatePartialCorrelationsTable(modelContainer, model, dataset, options, position = 6)

  if (options$covarianceMatrix && is.null(modelContainer[["coeffCovMatrixTable"]]))
    jaspRegression:::.linregCreateCoefficientsCovarianceMatrixTable(modelContainer, model, options, position = 7)

  if (options$collinearityDiagnostic && is.null(modelContainer[["collinearityTable"]]))
    jaspRegression:::.linregCreateCollinearityDiagnosticsTable(modelContainer, model, options, position = 8)
}
