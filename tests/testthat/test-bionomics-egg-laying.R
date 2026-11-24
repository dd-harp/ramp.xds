test_that("static egg batch setup accepts numeric values and options", {
  for (nPatches in c(1, 3)) {
    model <- make_xds_object_template(nPatches=nPatches, membership=seq_len(nPatches),
                                     residence=seq_len(nPatches))
    model$MY_obj <- list(setup_nu_obj(rep(0.2, nPatches), list()),
                         setup_nu_obj(rep(0.4, nPatches), list()))
    values <- seq_len(nPatches)/10
    inputs <- list(values, matrix(values, ncol=1))
    if (nPatches==1) inputs <- c(inputs, list(0.3))
    for (input in inputs) {
      result <- setup_F_nu(input, model, s=2)
      expect_identical(F_nu(0, result, 2), input)
      expect_identical(result$MY_obj[[2]]$nu, input)
      expect_identical(result$MY_obj[[2]]$nu_t, input)
      expect_identical(class(result$MY_obj[[2]]$nu_obj), "static")
      expect_identical(result$MY_obj[[1]], model$MY_obj[[1]])
      expect_identical(setup_nu(input, model, s=2), result)
    }
    expected <- setup_F_nu(values, model, s=2)
    expect_identical(setup_F_nu("as_is", model, list(nu=values), s=2), expected)
    expect_identical(setup_F_nu(list(name="as_is", nu=values), model, s=2), expected)
    expect_identical(setup_F_nu(rep(0.9, nPatches), model, list(nu=values), s=2),
                     expected)
    expect_identical(setup_F_nu(values, model), setup_F_nu(values, model, s=1))
    expect_identical(model$MY_obj[[2]]$nu, rep(0.4, nPatches))
  }
})

test_that("static egg batch setup reports missing and invalid inputs", {
  model <- make_xds_object_template()
  model$MY_obj <- list(setup_nu_obj(0.2, list()))
  expect_error(setup_F_nu("as_is", model), "Supply oviposition rates")
  expect_error(setup_F_nu("as_is", model, list(nu="invalid")), "is.numeric")
  expect_error(setup_nu(-0.1, model), "nu >= 0", fixed=TRUE)
  expect_error(setup_F_nu(c(0.1, 0.2), model), "length")
})

test_that("random egg batch setup uses the configured generator and species", {
  generator <- function(n, p1, p2) seq_len(n)*p1+p2
  for (nPatches in c(1, 3)) {
    model <- make_xds_object_template(nPatches=nPatches, membership=seq_len(nPatches),
                                     residence=seq_len(nPatches))
    model$MY_obj <- list(setup_nu_obj(rep(0.2, nPatches), list()),
                         setup_nu_obj(rep(0.4, nPatches), list()))
    options <- list(f_rand=generator, p1=0.1, p2=0.2)
    config <- make_F_nu_random(nPatches, options)
    expect_identical(config$f_rand, generator)
    expect_null(config$F_nu)
    expect_identical(class(config), "static")
    expect_identical(config$nu, generator(nPatches, 0.1, 0.2))
    expect_identical(make_F_nu_random(nPatches, f_rand=generator, p1=0.1, p2=0.2),
                     config)
    result <- setup_F_nu("random", model, options, s=2)
    expect_identical(result$MY_obj[[2]]$nu_obj, config)
    expect_identical(result$MY_obj[[1]], model$MY_obj[[1]])
    for (t in c(0, 10)) {
      expect_identical(F_nu(t, result, s=2), generator(nPatches, 0.1, 0.2))
      expect_identical(F_nu(t, result, s=1), rep(0.2, nPatches))
    }
    expect_identical(setup_F_nu(list(name="random", f_rand=generator, p1=0.1, p2=0.2),
                              model, s=2), result)
    expect_identical(model$MY_obj[[2]]$nu_obj$nu, rep(0.4, nPatches))

    set.seed(123)
    expected <- stats::rbeta(nPatches, 980, 1000)
    set.seed(123)
    result <- setup_F_nu("random", model)
    expect_identical(F_nu(0, result, s=1), expected)
  }
})
