test_that("remaining species interfaces put options before a default index", {
  families <- list(
    change_MY_inits=c("basicM", "GeM", "SI", "macdonald", "RMdts", "trivial"),
    setup_L_inits=c("basicL", "trivial"),
    change_L_inits=c("basicL", "trivial"),
    setup_exposure=c("nb", "pois")
  )
  for (name in names(families)) {
    generic <- get(name)
    args <- formals(generic)
    index <- if (name == "setup_exposure") "i" else "s"
    expect_lt(match("options", names(args)), match(index, names(args)))
    expect_identical(args[[index]], 1)
    expect_identical(args$options, quote(list()))
    for (module in families[[name]]) {
      expect_identical(formals(getS3method(name, module)), args)
    }
  }

  for (name in c("setup_V_obj", "change_V_pars", "setup_V_inits", "change_V_inits")) {
    args <- formals(get(name))
    expect_lt(match("options", names(args)), match("i", names(args)))
    expect_identical(args$i, 1)
    expect_identical(args$options, quote(list()))
  }
  expect_identical(names(formals(setup_V_inits)), c("xds_obj", "H", "options", "i"))
})

test_that("initial-value changes preserve positional, named and species behavior", {
  model <- xds_setup(MYname="SI", Lname="basicL")
  for (name in c("change_MY_inits", "setup_L_inits", "change_L_inits")) {
    fun <- get(name)
    is_MY <- name == "change_MY_inits"
    component <- if (is_MY) "MY_obj" else "L_obj"
    options <- if (is_MY) list(M=20, Y=3) else list(L=20)
    model2 <- if (is_MY) setup_MY_obj("SI", model, s=2) else
      setup_L_obj("basicL", 1, model, s=2)
    changed <- fun(model2, options)
    expect_equal(changed, fun(model2, options=options, s=1))
    expect_equal(changed[[component]][[1]]$inits, options)
    expect_equal(fun(model2), fun(model2, list(), 1))
    second <- fun(model2, options, 2)
    expect_equal(second[[component]][[1]], model2[[component]][[1]])
    expect_equal(second[[component]][[2]]$inits, options)
  }
})

test_that("exposure setup accepts options before the host index", {
  model <- xds_setup()
  for (module in c("nb", "pois")) {
    options <- list(sz=0.4)
    changed <- setup_exposure(module, model, options)
    expect_equal(changed, setup_exposure(module, model, options=options, i=1))
    expect_equal(setup_exposure(module, model), setup_exposure(module, model, list(), 1))
    second <- setup_exposure(module, model, options, 2)
    expect_equal(second$env_het_obj[[1]], model$env_het_obj[[1]])
    expect_s3_class(second$env_het_obj[[2]], module)
    if (module == "nb") expect_equal(second$env_het_obj[[2]]$sz, 0.4)
  }
})
