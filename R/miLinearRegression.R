#
# Copyright (C) 2026 Utrecht University
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

### ------------------------------------------------------------------------------------------------------------------###

.runRegression <- function(jaspResults, options, ready, lmFunction) {
  impData <- jaspResults[["MiceMids"]]$object |> mice::complete("all")

  # modelContainer <- .linregGetModelContainer(jaspResults, position = 1)
  modelContainer <- jaspResults[["ModelContainer"]]
  model <- jaspRegression:::.linregCalcModel(modelContainer, impData, options, ready, lmFunction)

  model[[1]][["rSquareChange"]] <- .pooledRSquaredChange(fit1 = model[[1]]$fit) # , fStat = options$fStat)

  for (i in seq_along(model)[-1]) {
    model[[i]][["rSquareChange"]] <-
      .pooledRSquaredChange(fit1 = model[[i]]$fit, fit0 = model[[i - 1]]$fit) # , fStat = options$fStat)
    # durbinWatson  <- model[[i]][["durbinWatson"]]
  }

  if (is.null(modelContainer[["summaryTable"]])) {
    jaspRegression:::.linregCreateSummaryTable(modelContainer, model, options, position = 1)
  }

  # TODO (KML): Check the R2, F, AIC/BIC stuff and put MI appropriate versions in
  # TODO (KML): Add footnotes about pooling

  if (options$modelFit && is.null(modelContainer[["anovaTable"]])) {
    jaspRegression:::.linregCreateAnovaTable(modelContainer, model, options, position = 2)
  }

  # TODO (KML): Check the ANOVA stats.
  #             - Are they correct for MI data
  #             - Do they correctly adjust for D1, D2, D3?

  if (options$coefficientEstimate && is.null(modelContainer[["coeffTable"]])) {
    jaspRegression:::.linregCreateCoefficientsTable(modelContainer, model, impData, options, position = 3)
  }

  # TODO (KML): Check what we can do about the bootstrapping, partial cor, and collinearity tables

  # if (options$coefficientBootstrap && is.null(modelContainer[["bootstrapCoeffTable"]]))
  #   jaspRegression:::.linregCreateBootstrapCoefficientsTable(modelContainer, model, dataset, options, position = 4)

  # if (options$partAndPartialCorrelation && is.null(modelContainer[["partialCorTable"]]))
  #   jaspRegression:::.linregCreatePartialCorrelationsTable(modelContainer, model, dataset, options, position = 6)

  if (options$covarianceMatrix && is.null(modelContainer[["coeffCovMatrixTable"]])) {
    jaspRegression:::.linregCreateCoefficientsCovarianceMatrixTable(modelContainer, model, options, position = 4)
  }

  # if (options$collinearityDiagnostic && is.null(modelContainer[["collinearityTable"]]))
  #   jaspRegression:::.linregCreateCollinearityDiagnosticsTable(modelContainer, model, options, position = 8)
}

## Execute .runRegression() within the 'jaspRegression' namespace:
# environment(.runRegression) <- asNamespace("jaspRegression")

### ------------------------------------------------------------------------------------------------------------------###

.pooledRSquaredChange <- function(fit1, fit0 = NULL) {
  # fFun <- switch(fStat,
  #   d1 = mice::D1,
  #   d2 = mice::D2,
  #   d3 = mice::D3
  # )
  #
  if (is.null(fit0)) {
    out <- list(
      R2c = NA,
      Fc  = NA,
      df1 = 0,
      df2 = fit1$df.residual,
      p   = NA
    )
  } else {
    fOut <- fit1$fFun(fit1 = fit1$fits, fit0 = fit0$fits)

    out <- list(
      R2c = fit1$pooled$r2[1, "est"] - fit0$pooled$r2[1, "est"],
      Fc  = fOut$result[[1]],
      df1 = fOut$result[[2]],
      df2 = fOut$result[[3]],
      p   = fOut$result[[4]]
    )
  }

  out
}

# environment(.pooledRSquaredChange) <- asNamespace("jaspRegression")

### ------------------------------------------------------------------------------------------------------------------###
