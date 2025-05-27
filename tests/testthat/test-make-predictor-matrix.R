test_that("multiplication works", {
  dataset <- mice::boys
  options <- jaspTools::analysisOptions("MissingDataImputation")
  expect_equal(
    .makePredictorMatrix(dataset, options),
    mice::make.predictorMatrix(dataset)
  )
  options$quickpred <- TRUE
  expect_equal(
    .makePredictorMatrix(dataset, options),
    mice::quickpred(dataset)
  )
})
