test_that("MY parameter interfaces put options before the species index", {
  expect_identical(names(formals(change_MY_pars)), c("xds_obj", "options", "s"))
  for (module in c("basicM", "GeM", "SI", "macdonald", "trivial")) {
    method <- getS3method("change_MY_pars", module)
    expect_identical(formals(method), formals(change_MY_pars))
  }
})

test_that("MY parameter changes accept options before the species index", {
  model <- xds_setup(MYname="macdonald")
  options <- list(f=0.4)

  positional <- change_MY_pars(model, options)
  expect_equal(positional, change_MY_pars(model, options=options, s=1))
  expect_equal(positional$MY_obj[[1]]$f, 0.4)
  expect_equal(change_MY_pars(model), change_MY_pars(model, list(), 1))

  model <- setup_MY_obj("macdonald", model, s=2)
  second <- change_MY_pars(model, options, 2)
  expect_equal(second$MY_obj[[1]], model$MY_obj[[1]])
  expect_equal(second$MY_obj[[2]]$f, 0.4)
})
