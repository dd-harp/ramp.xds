test_that("Births methods receive the model state vector", {
  Births.probe <- function(t, y, xds_obj, i) {
    y
  }
  xds_obj <- list(
    XH_obj=list(list(birth_obj=structure(list(), class="probe")))
  )
  y <- c(10, 20, 30)

  expect_identical(Births(0, y, xds_obj, 1), y)
})

test_that("constant birth setup still works with state-dependent dispatch", {
  xds_obj <- make_indices(
    setup_XH_obj("SIS", 1, make_xds_object_template())
  )
  xds_obj <- setup_births(
    "constant",
    xds_obj,
    options=list(birth_rates=rep(0.1, xds_obj$nStrata[1]))
  )
  y <- get_inits(xds_obj, flatten=TRUE)

  expect_equal(
    Births(0, y, xds_obj, 1),
    rep(0.1, xds_obj$nStrata[1])
  )
})
