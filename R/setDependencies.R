

.setImputationDependencies <- function() {
  c(
    "imputationVariables",
    "passiveImputation",
    "changeFullModel",
    "changeNullModel",
    "visitSequence",
    "nImps",
    "nIters",
    "quickpred", 
    "quickpredMincor", 
    "quickpredMinpuc", 
    "quickpredMethod", 
    "quickpredIncludes", 
    "quickpredExcludes",
    "seed"
  )
}

.setModelDependencies <- function(analysis) {
  deps <- NULL
  if (analysis == "linreg") {
    deps <- c(
      "dependent",
      "method",
      "covariates",
      "factors",
      "weights",
      "modelTerms",
      "steppingMethodCriteriaType",
      "steppingMethodCriteriaPEntry",
      "steppingMethodCriteriaPRemoval",
      "steppingMethodCriteriaFEntry",
      "steppingMethodCriteriaFRemoval",
      "interceptTerm",
      "quadraticTerms",
      "fStat",
      "llEst"
    )
  }
  c(
    "analysis",
    deps
  )
}