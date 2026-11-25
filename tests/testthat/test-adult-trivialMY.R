test_that("trivialMY preserves its constructor and limits legacy dispatch", {
  module <- make_MY_obj_trivial(2, list(f=c(0.2, 0.3)))
  expect_identical(class(module), "trivialMY")
  expect_identical(class(module$baseline), c("static", "trivialMY"))
  generics <- c(
    "check_MY", "F_fqZ", "F_eggs", "F_fqM", "MBionomics", "MEffectSizes",
    "dMYdt", "Update_MYt", "setup_MY_obj", "get_MY_vars", "get_MY_pars",
    "change_MY_inits", "change_MY_pars", "get_skillset_MY", "setup_MY_inits",
    "setup_MY_ix", "parse_MY_orbits", "get_f", "get_q", "get_g", "get_sigma"
  )
  for (generic in generics) {
    expect_true(is.function(getS3method(generic, "trivialMY")))
    if (generic %in% c("setup_MY_obj", "get_skillset_MY")) {
      expect_true(is.function(getS3method(generic, "trivial")))
    } else {
      expect_null(getS3method(generic, "trivial", optional=TRUE))
    }
  }
  expect_true("make_MY_obj_trivial" %in% getNamespaceExports("ramp.xds"))
  expect_null(get_MY_inits(list(MY_obj=list(module))))
  expect_identical(get_skillset_MY("trivial"), get_skillset_MY("trivialMY"))
})

test_that("trivialMY aliases create equivalent canonical models", {
  args <- list(nPatches=2, membership=1:2, residence=1:2, Lname="basicL",
               MYoptions=list(f=c(0.2, 0.3), q=c(0.5, 0.8), Z=c(2, 4), eggs=c(10, 20)))
  model <- do.call(xds_setup, c(list(MYname="trivialMY"), args))
  expect_no_warning(compat <- do.call(xds_setup, c(list(MYname="trivial"), args)))
  expect_identical(compat, model)
  expect_identical(model$MYname, "trivialMY")
  expect_identical(get_skillset_MY(model), get_skillset_MY("trivialMY"))
  expect_identical(xds_solve(compat, Tmax=2), xds_solve(model, Tmax=2))

  y <- get_inits(model, flatten=TRUE)
  expect_equal(F_fqZ(0, y, model, 1), c(0.2, 0.96))
  expect_equal(F_eggs(0, y, model, 1), c(10, 20))
  expect_identical(F_fqM(0, y, model, 1), numeric())
  expect_equal(get_f(model), c(0.2, 0.3))
  expect_equal(get_q(model), c(0.5, 0.8))
  expect_identical(get_g(model), numeric())
  expect_identical(get_sigma(model), numeric())
  expect_equal(get_MY_pars(model)$Z, c(2, 4))
  expect_identical(get_MY_vars(y, model, 1), list())
  expect_identical(dMYdt(0, y, model, 1), numeric())
  expect_identical(Update_MYt(0, y, model, 1), numeric())
  expect_identical(MBionomics(0, y, model, 1), model)
  expect_identical(MEffectSizes(0, y, model, 1), model)
  expect_identical(check_MY(model, c(0, 2), 1), model)
  expect_identical(setup_MY_inits(model), model)
  expect_identical(change_MY_inits(model), model)
  expect_identical(setup_MY_ix(model, 1), model)
  expect_identical(parse_MY_orbits(matrix(y, nrow=1), model, 1), list())
  changed <- change_MY_pars(model, list(F_season=function(t, vars) 2))
  expect_equal(F_eggs(0, y, changed, 1), c(20, 40))

  for (s in 1:2) {
    options <- list(Z=c(3, 5), eggs=c(30, 50))
    named <- setup_MY_obj("trivialMY", model, options, s)
    expect_no_warning(aliased <- setup_MY_obj("trivial", model, options, s))
    expect_identical(aliased, named)
    expect_identical(class(aliased$MY_obj[[s]]), "trivialMY")
    expect_equal(aliased$MY_obj[[s]]$Z, options$Z)
    expect_equal(aliased$MY_obj[[s]]$eggs, options$eggs)
    expect_identical(get_skillset_MY(aliased, s), get_skillset_MY("trivialMY"))
  }
})

test_that("isolated setups use trivialMY and leave other module names unchanged", {
  for (model in list(xds_setup_aquatic(), xds_setup_human(), xds_setup_eir())) {
    expect_identical(model$MYname, "trivialMY")
    expect_identical(class(model$MY_obj[[1]]), "trivialMY")
    solved <- xds_solve(model, Tmax=2)
    expect_equal(solved$outputs$time, 0:2)
    expect_true(all(is.finite(solved$outputs$deout)))
  }
  model <- xds_setup(XHname="trivialXH", MYname="trivialMY")
  expect_identical(class(model$XH_obj[[1]]), "trivialXH")
  expect_identical(class(model$L_obj[[1]]), "trivialL")
  expect_identical(get_skillset_XH("trivial"), get_skillset_XH("trivialXH"))
  expect_type(get_skillset_L("trivial"), "list")
})
