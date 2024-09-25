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
  options <- .initImputationOptions(jaspResults, options)

  ready <- length(options$variables) > 0
  if(ready) {
    # read dataset
    dataset <- .readData(options)
    # error checking
    errors <- .errorHandling(dataset, options)

    # Output containers, tables, and plots based on the results. These functions should not return anything!
    .createMainContainer(jaspResults, options)

    .imputeMissingData(jaspResults, options, dataset)

    if(options$tracePlot)
      .createTracePlot(jaspResults, options, dataset)
    if(options$densityPlot)
      .createDensityPlot(jaspResults, options, dataset)
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

.createMainContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["mainContainer"]])) return()
  
  mainContainer <- createJaspContainer("Missing Data Analysis")
  mainContainer$dependOn(options = c("variables", "groupVar"))
  
  jaspResults[["mainContainer"]] <- mainContainer
}

###-Init Functions-----------------------------------------------------------------------------------------------------------###

.initImputationOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis
  options
}

###-Output Functions---------------------------------------------------------------------------------------------------------###

#' @importFrom mice mice complete
.imputeMissingData <- function(jaspResults, options, dataset) {

  jaspResults[["miceMids"]] <- miceMids <- createJaspState()
  miceMids$dependOn(options = c("variables", "nImps", "nIter", "seed"))

  miceOut <- with(options,
                  mice(data  = dataset[ , encodeColNames(variables), drop = FALSE],
                       m     = nImp,
                       maxit = nIter,
                       seed  = seed,
                       print = FALSE)
  )
  miceMids$object <- miceOut

  ### NOTE: This function seems to work, but I'm not sure of what to do with the imputed data.
  ###       Are we able to overwrite the underlying 'dataset' object (and do we want to do so)?
  ###       I guess, that's a discussion we need to have with the Joris
}

###--------------------------------------------------------------------------------------------------------------------------###

### NOTE: The two following functions don't work yet. ggmice isn't happy about getting a JASP-flavoured R6 object as input.

#' @importFrom ggmice plot_trace
.createTracePlot <- function(jaspResults, options, dataset) {
  if (!is.null(jaspResults[["tracePlot"]])) return()

  tracePlot <- createJaspPlot(title = "Trace Plot", height = 320, width = 480)
  tracePlot$dependOn(options = "variables")
  
  # Bind plot to jaspResults
  jaspResults[["tracePlot"]] <- tracePlot

  tracePlot$plotObject <- jaspResults[["miceMids"]] |> plot_trace() 
}

###--------------------------------------------------------------------------------------------------------------------------###

#' @importFrom ggmice densityplot
#' @importFrom ggplot2 aes geom_density
.createDensityPlot <- function(jaspResults, options, dataset) {

  jaspResults[["densityPlots"]] <- createJaspContainer("Density Plots")

  for(v in options$variables) {
    densityPlot <- createJaspPlot(title = v, height = 320, width = 480)
    densityPlot$dependOn(options = "variables")

    ## Bind the density plot for variable 'v' to the 'densityPlots' container in jaspResults
    jaspResults[["densityPlots"]][[v]] <- densityPlot

    ## Populate the plot object
    densityPlot$plotObject <- jaspResults[["miceMids"]] |> ggmice(aes(x = v, group = .imp)) + geom_density()
  }
}

###--------------------------------------------------------------------------------------------------------------------------###

.runRegression <- function(jaspResults, options, offset = 2) {
  modelContainer  <- .linregGetModelContainer(jaspResults, position = offset + 1)
  model <- jaspResults[["miceMids"]] |>
    complete(1) |>
    .linregCalcModel(modelContainer, dataset = _, options, ready)

  # if (is.null(modelContainer[["summaryTable"]]))
  #   .linregCreateSummaryTable(modelContainer, model, options, position = 1)
}
