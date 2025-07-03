context("Missing Data Imputation")

boysdata <- read.csv("tests/testthat/boys.csv", na.strings = c("", "NA", "N/A", "<NA>"), stringsAsFactors = FALSE)

boysdata$gen <- as.ordered(boysdata$gen)
boysdata$phb <- as.ordered(boysdata$phb)
boysdata$reg <- as.factor(boysdata$reg)

options <- jaspTools::analysisOptions("MissingDataImputation")

options$imputationVariables <- list(
  optionKey = "variable",
  types = c("scale", "scale", "scale", "scale", "scale", "ordinal", "ordinal", "scale", "nominal"),
  value = list(
    list(method = "pmm", variable = "age"),
    list(method = "pmm", variable = "hgt"),
    list(method = "pmm", variable = "wgt"),
    list(method = "pmm", variable = "bmi"),
    list(method = "pmm", variable = "hc"),
    list(method = "polr", variable = "gen"),
    list(method = "polr", variable = "phb"),
    list(method = "pmm", variable = "tv"),
    list(method = "polyreg", variable = "reg")
  )
)

result <- jaspTools::runAnalysis("MissingDataImputation", boysdata, options)

imp <- mice::mice(
  boysdata,
  m = options$nImp,
  maxit = options$nIter,
  method = sapply(options$imputationVariables$value, \(x) x$method),
  formulas = result$state$other$state_18$formulas,
  seed = options$seed,
)

options

test_that("imputation output is equal", {
  expect_equal(result$state$other$state_18$imp$reg, imp$imp$reg)
})
