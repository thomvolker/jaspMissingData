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

    # browser() #########################################################################################################

    .initMiceMids(jaspResults)
    options <- .imputeMissingData(jaspResults[["MiceMids"]], options, dataset)

    ## Initialize containers to hold the convergence plots and analysis results:
    # convergencePlots  <- .initConvergencePlots(jaspResults)
    # analysisContainer <- .initAnalysisContainer(jaspResults)
    .initConvergencePlots(jaspResults)
    .initAnalysisContainer(jaspResults)

    if(options$tracePlot)
      .createTracePlot(jaspResults[["ConvergencePlots"]], jaspResults[["MiceMids"]])
    if(options$densityPlot)
      .createDensityPlot(jaspResults[["ConvergencePlots"]], jaspResults[["MiceMids"]], options)

    if(options$runLinearRegression) {
      miFits <- .estimateRegressionModels(jaspResults[["MiceMids"]], options)
      saveRDS(miFits, "/home/kylelang/software/jasp/modules/imputation/data/miFits.rds")
      modelContainer <- .poolRegressionEstimates(jaspResults, miFits, options, offset = 0)
      .populateRegressionResults(jaspResults, modelContainer, options)
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

  options
}

###------------------------------------------------------------------------------------------------------------------###

.initMiceMids <- function(jaspResults) {
  if (!is.null(jaspResults[["MiceMids"]])) return()

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

#' @importFrom mice mice complete
.imputeMissingData <- function(miceMids, options, dataset) {

  miceOut <- try(
    with(options,
      mice(
        # data  = dataset[ , encodeColNames(variables), drop = FALSE],
        data  = dataset[ , variables, drop = FALSE],
        m     = nImp,
        maxit = nIter,
        seed  = seed,
        print = FALSE
      )
    )
  )

  if (!inherits(miceOut, "try-error")) {
    saveRDS(miceOut, "/home/kylelang/software/jasp/modules/imputation/data/miceOut.rds")
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

#' @importFrom ggmice plot_trace
.createTracePlot <- function(convergencePlots, miceMids) {
  # if (!is.null(jaspResults[["tracePlot"]])) return()

  tracePlot <- createJaspPlot(title = "Trace Plot", height = 320, width = 480)
  # tracePlot$dependOn(options = "variables")
  
  # Bind plot to jaspResults via the convergencePlots container:
  convergencePlots[["TracePlot"]] <- tracePlot

  tracePlot$plotObject <- miceMids$object |> plot_trace()
}

###------------------------------------------------------------------------------------------------------------------###

#' @importFrom ggmice ggmice densityplot
#' @importFrom ggplot2 aes geom_density
.createDensityPlot <- function(convergencePlots, miceMids, options) {

  convergencePlots[["DensityPlots"]] <- createJaspContainer("Density Plots")

  for(v in options$imputedVariables) {
    densityPlot <- createJaspPlot(title = v, height = 320, width = 480)
    # densityPlot$dependOn(options = "variables")

    ## Bind the density plot for variable 'v' to the 'densityPlots' container in jaspResults
    convergencePlots[["DensityPlots"]][[v]] <- densityPlot

    ## Populate the plot object
    densityPlot$plotObject <- miceMids$object |> ggmice(aes(x = .data[[v]], group = .imp)) + geom_density()
  }
}

###------------------------------------------------------------------------------------------------------------------###

.estimateRegressionModels <- function(miceMids, options) {
  ready <- inherits(miceMids$object, "mids") && # We can't do an analysis before imputing
    options$dependent != "" &&
    (length(unlist(options$modelTerms)) > 0 || options$interceptTerm)

  # browser() ############################################################################################################

  dummyContainer <- createJaspContainer()
  outData <- list()
  models <- list()
  for (m in 1:options$nImp) {
    dataset <- outData[[m]] <- miceMids$object |> complete(m) 
    model   <- createJaspContainer() |> jaspRegression:::.linregCalcModel(dataset, options, ready)

    models[[m]] <- model

    if (m == 1) {
      meta <- lapply(model, "[", x = c("predictors", "number", "title"))
      fits <- vector("list", length(model))
    }

    for (i in seq_along(model)) fits[[i]][[m]] <- model[[i]]$fit
    
  }

  saveRDS(outData, "/home/kylelang/software/jasp/modules/imputation/data/outData.rds")
  saveRDS(models, "/home/kylelang/software/jasp/modules/imputation/data/models.rds")

  list(meta = meta, fits = fits)
}

###------------------------------------------------------------------------------------------------------------------###

.poolRegressionEstimates <- function(jaspResults, miFits, options, offset) {

  # browser() ############################################################################################################

  modelContainer <- jaspRegression:::.linregGetModelContainer(jaspResults, position = offset + 1)

  models <- miFits$meta
  for (i in seq_along(miFits$fits)) {
    pooledFit <- pooledLmObject(miFits$fits[[i]], fType = options$fType)
    models[[i]]$fit <- pooledFit
  }

  modelContainer[["models"]] <- models
}

###------------------------------------------------------------------------------------------------------------------###

.populateRegressionResults <- function(jaspResults, modelContainer, options) {

  # browser() ############################################################################################################

  model <- modelContainer[["models"]]

  if (is.null(modelContainer[["summaryTable"]]))
    jaspRegression:::.linregCreateSummaryTable(modelContainer, model, options, position = 1)

  # if (options$modelFit && is.null(modelContainer[["anovaTable"]]))
  #   jaspRegression:::.linregCreateAnovaTable(modelContainer, model, options, position = 2)

  # if (options$coefficientEstimate && is.null(modelContainer[["coeffTable"]]))
  #   jaspRegression:::.linregCreateCoefficientsTable(modelContainer, model, dataset, options, position = 3)

  # if (options$coefficientBootstrap && is.null(modelContainer[["bootstrapCoeffTable"]]))
  #   jaspRegression:::.linregCreateBootstrapCoefficientsTable(modelContainer, model, dataset, options, position = 4)

  # if (options$partAndPartialCorrelation && is.null(modelContainer[["partialCorTable"]]))
  #   jaspRegression:::.linregCreatePartialCorrelationsTable(modelContainer, model, dataset, options, position = 6)

  # if (options$covarianceMatrix && is.null(modelContainer[["coeffCovMatrixTable"]]))
  #   jaspRegression:::.linregCreateCoefficientsCovarianceMatrixTable(modelContainer, model, options, position = 7)

  # if (options$collinearityDiagnostic && is.null(modelContainer[["collinearityTable"]]))
  #   jaspRegression:::.linregCreateCollinearityDiagnosticsTable(modelContainer, model, options, position = 8)
}

###------------------------------------------------------------------------------------------------------------------###

.lmFun <- function(formula, data, options, ...) {
  ## If 'data' is a mids object, broadcast the model estimation across all imputed datasets
  if (inherits(data, "mids")) {
    return(
      with(data, stats::lm(formula = as.formula(formula), ...)) |> pooledLmObject(fType = options$fType)
    )
  } else { # Otherwise, default to basic lm()
    return(
      stats::lm(formula = formula, data = data, ...)
    )
  }
}

# options <- list(fType = 3)

# getwd()
# miceOut <- readRDS("../data/miceOut.rds")
# formula <- "hgt ~ wgt + hc + reg"

# class(miceOut)
# tmp <- .lmFun(formula, miceOut, options)
# tmp

# s1 <- summary(tmp)
# s1

# class(s1)

# summary(tmp) |> print()

# tmp |> class()

# summary(tmp) |> print()

# coef(tmp)

# summary(tmp)$r.squared

# .lmFun(formula, complete(miceOut, 1), options)

# class(tmp[[2]])

# tmp
# pool(tmp)

# tmp$analyses |> class()
