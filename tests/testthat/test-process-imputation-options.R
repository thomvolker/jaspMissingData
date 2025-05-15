test_that(".processImputationOptions", {
  options <- list(
    imputationVariables = list(
      optionKey = "variable",
      types = c("scale", "nominal", "scale"),
      value = list(
        list(method = "pmm", variable = "hgt"),
        list(method = "logreg", variable = "reg"),
        list(method = "pmm", variable = "bmi")
      )
    )
  )
  expect_equal(
    .processImputationOptions(options)$imputationTargets,
    c("hgt", "reg", "bmi")
  )
  expect_equal(
    .processImputationOptions(options)$imputationMethods,
    c(hgt = "pmm", reg = "logreg", bmi = "pmm")
  )

})
