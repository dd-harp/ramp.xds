test_that("MY initial-value interfaces put options before a default species index", {
  expect_identical(names(formals(setup_MY_inits)), c("xds_obj", "options", "s"))
  expect_identical(formals(setup_MY_inits)$s, 1)
  expect_identical(formals(setup_MY_inits)$options, quote(list()))
  for (module in c("basicM", "GeM", "SI", "macdonald", "RMdts", "trivial")) {
    expect_identical(formals(getS3method("setup_MY_inits", module)),
                     formals(setup_MY_inits))
  }
})

test_that("MY initial values accept positional, named and default arguments", {
  model <- xds_setup(MYname="SI")
  options <- list(M=20, Y=3)
  positional <- setup_MY_inits(model, options)
  expect_equal(positional, setup_MY_inits(model, options=options, s=1))
  expect_equal(positional$MY_obj[[1]]$inits, list(M=20, Y=3))
  expect_equal(setup_MY_inits(model), setup_MY_inits(model, list(), 1))

  model <- setup_MY_obj("SI", model, s=2)
  second <- setup_MY_inits(model, options, 2)
  expect_equal(second$MY_obj[[1]], model$MY_obj[[1]])
  expect_equal(second$MY_obj[[2]]$inits, list(M=20, Y=3))
})
