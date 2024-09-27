#
# Copyright (C) 2018 University of Amsterdam
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

#' Inspect the missing data problem
#' @export
MissingDataInspection <- function(jaspResults, dataset, options) {
  # Set title
  jaspResults$title <- "Missing Data Inspection"
  # Init options: add variables to options to be used in the remainder of the analysis
  options <- .initInspectionOptions(jaspResults, options)

  ready <- length(options$variables) > 0
  if(ready) {
    # read dataset
    dataset <- .readData(options)
    # error checking
    errors <- .errorHandling(dataset, options)

    # Compute (a list of) results from which tables and plots can be created
    #localResults <- .computeResults(jaspResults, dataset, options)

    if(options$patternPlot) # always active by default
      .createPatternPlot(jaspResults, options, dataset)

    # Output containers, tables, and plots based on the results. These functions should not return anything!
    .createMainContainer(jaspResults, options)

    if(options$fluxTable)
      .createFluxTable(jaspResults, options, dataset)
    if(options$fluxPlot)
      .createFluxPlot(jaspResults, options, dataset)
    if(options$corrPlot)
      .createCorrPlot(jaspResults, options, dataset)
  }
    return()
}

###-Common Functions (We shouldn't be copying these)-------------------------------------------------------------------------###

# .readData <- function(options) {
#   vars <- unlist(options$variables)
#   # Read in the dataset using the built-in functions
#   if (!is.null(options$groupVar) && options$groupVar != "")
#     .readDataSetToEnd(columns = vars, columns.as.factor = options$groupVar)
#   else
#     .readDataSetToEnd(columns = vars)
# }

# ###--------------------------------------------------------------------------------------------------------------------------###

# .errorHandling <- function(dataset, options)
#   .hasErrors(dataset,
#              "run",
#              type = c('observations', 'variance', 'infinity'),
#              all.target = options$variables,
#              observations.amount = '< 2',
#              exitAnalysisIfErrors = TRUE)

###--------------------------------------------------------------------------------------------------------------------------###

.createMainContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["mainContainer"]])) return()

  mainContainer <- createJaspContainer("Missing Data Analysis")
  mainContainer$dependOn(options = c("variables", "groupVar"))

  jaspResults[["mainContainer"]] <- mainContainer
}

###-Init Functions-----------------------------------------------------------------------------------------------------------###

.initInspectionOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis
  options
}

###-Results Functions--------------------------------------------------------------------------------------------------------###

#.computeResults <- function(jaspResults, dataset, options) {
#  if (is.null(jaspResults[["stateResults"]])) {
#    results <- .resultsHelper(dataset, options)
#
#    jaspResults[["stateResults"]] <- createJaspState(results)
#    jaspResults[["stateResults"]]$dependOn(options = c("variables", "groupVar"))
#
#  } else {
#    results <- jaspResults[["stateResults"]]$object
#  }
#
#  results
#}

###--------------------------------------------------------------------------------------------------------------------------###

#.resultsHelper <- function(dataset, options) {
#  mMat     <- is.na(dataset)
#  coverMat <- mice::md.pairs(dataset)$rr / nrow(dataset)
#
#  list(pm = colMeans(mMat),
#       cm = colSums(mMat),
#       cc = coverMat,
#       flux = mice::flux(dataset)
#       )
#}

###-Output Functions-------------------------------------------------------------------------------------------------###

#' @importFrom mice flux
.createFluxTable <- function(jaspResults, options, dataset) {
  if (!is.null(jaspResults[["mainContainer"]][["fluxTable"]])) return()

  # Below is one way of creating a table
  table <- createJaspTable(title = "In/Out-Flux Statistics")
  table$dependOn(options = c("variables", "groupVar")) # not strictly necessary because container

  # Bind table to jaspResults
  jaspResults[["mainContainer"]][["fluxTable"]] <- table

  # Add column info
  table$addColumnInfo(name = "var",     title = "Variable",            type = "string"                 )
  table$addColumnInfo(name = "pobs",    title = "Proportion Observed", type = "number", format = "sf:4")
  table$addColumnInfo(name = "influx",  title = "Influx",              type = "number", format = "sf:4")
  table$addColumnInfo(name = "outflux", title = "Outflux",             type = "number", format = "sf:4")
  table$addColumnInfo(name = "ainb",    title = "Average Inbound",     type = "number", format = "sf:4")
  table$addColumnInfo(name = "aout",    title = "Average Outbound",    type = "number", format = "sf:4")
  table$addColumnInfo(name = "fico",    title = "FICO",                type = "number", format = "sf:4")

  ## Compute flux stats:
  flux <- dataset[ , encodeColNames(options$variables), drop = FALSE] |> flux()

  ## Add the flux stats to the JASP table:
  table[["var"]] <- rownames(flux)
  with(flux, {
         table[["pobs"]]    <- pobs
         table[["influx"]]  <- influx
         table[["outflux"]] <- outflux
         table[["ainb"]]    <- ainb
         table[["aout"]]    <- aout
         table[["fico"]]    <- fico
       }
  )
}

###------------------------------------------------------------------------------------------------------------------###

#' @importFrom ggmice plot_flux
.createFluxPlot <- function(jaspResults, options, dataset) {
  if (!is.null(jaspResults[["fluxPlot"]])) return()

  fluxPlot <- createJaspPlot(title = "Influx/Outflux Plot", height = 320, width = 480)
  fluxPlot$dependOn(options = c("variables", "groupVar"))

  # Bind plot to jaspResults
  jaspResults[["fluxPlot"]] <- fluxPlot

  fluxPlot$plotObject <- dataset[ , encodeColNames(options$variables), drop = FALSE] |> ggmice::plot_flux()
}

###------------------------------------------------------------------------------------------------------------------###

#' @importFrom ggmice plot_corr
.createCorrPlot <- function(jaspResults, options, dataset) {
  if (!is.null(jaspResults[["corrPlot"]])) return()

  corrPlot <- createJaspPlot(title = "Correlation Plot", height = 320, width = 480)
  corrPlot$dependOn(options = c("variables", "groupVar"))

  # Bind plot to jaspResults
  jaspResults[["corrPlot"]] <- corrPlot

  corrPlot$plotObject <- dataset[ , encodeColNames(options$variables), drop = FALSE] |> ggmice::plot_corr()
}

###------------------------------------------------------------------------------------------------------------------###

#' @importFrom ggmice plot_pattern
.createPatternPlot <- function(jaspResults, options, dataset) {
  if (!is.null(jaspResults[["patternPlot"]])) return()

  patternPlot <- createJaspPlot(title = "Response Pattern Plot", height = 320, width = 480)
  patternPlot$dependOn(options = c("variables", "groupVar"))

  # Bind plot to jaspResults
  jaspResults[["patternPlot"]] <- patternPlot

  patternPlot$plotObject <- dataset[ , encodeColNames(options$variables), drop = FALSE] |> ggmice::plot_pattern()
}
