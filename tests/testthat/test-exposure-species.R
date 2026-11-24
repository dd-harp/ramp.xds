test_that("exposure initialization is separate from Poisson selection", {
  object <- make_xds_object_template()
  expect_s3_class(object$env_het_obj[[1]], "pois")
  expect_identical(object$terms$FoI, list(list()))
  expect_identical(object$terms$AR, list(list()))

  object$terms$FoI <- list(c(1, 2), 3)
  object$terms$AR <- list(c(0.1, 0.2), 0.3)
  object$env_het_obj[[1]] <- make_exposure_nb(2, list(sz = 4))
  original <- object
  result <- setup_exposure("pois", object)
  expect_s3_class(result$env_het_obj[[1]], "pois")
  expect_identical(result$env_het_obj[[1]]$sz, 4)
  expect_identical(result$terms, original$terms)

  result <- setup_exposure("pois", result, i = 2)
  expect_s3_class(result$env_het_obj[[2]], "pois")
  expect_identical(result$terms, original$terms)
  expect_s3_class(object$env_het_obj[[1]], "nb")
})

test_that("Exposure computes infection rates for every host species", {
  hosts <- lapply(1:2, function(i) {
    structure(list(
      time_away = c(0.25, 0.5),
      travel_eir = c(2, 4) * i,
      travel_eir_obj = structure(list(), class = "static")
    ), class = "trivialXH")
  })
  object <- list(
    nHostSpecies = 2,
    XH_obj = hosts,
    env_het_obj = list(make_exposure_pois(), make_exposure_pois()),
    terms = list(EIR = list(c(1, 3), c(5, 7)), FoI = list(), AR = list())
  )
  expected <- list(c(1.25, 3.5), c(4.75, 7.5))
  for (model in c("xde", "dts")) {
    object$xds <- structure(model, class = model)
    result <- Exposure(0, numeric(), object)
    expect_equal(result$terms$FoI, expected)
    if (model == "dts") {
      expect_equal(result$terms$AR, lapply(expected, function(h) 1 - exp(-h)))
    } else {
      expect_identical(result$terms$AR, object$terms$AR)
    }
    expect_identical(result$XH_obj, object$XH_obj)

    single <- object
    single$nHostSpecies <- 1
    result <- Exposure(0, numeric(), single)
    expect_equal(result$terms$FoI, expected[1])
  }
})
