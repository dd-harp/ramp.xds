test_that("trivialXH retains only setup and skill-set compatibility methods", {
  module <- make_XH_obj_trivial(2, list(H=c(100, 200), kappa=c(0.1, 0.2)))
  expect_identical(class(module), "trivialXH")
  generics <- c(
    "get_skillset_XH", "check_XH", "F_I", "F_H", "F_infectivity",
    "dXHdt", "Update_XHt", "F_prevalence", "F_ni",
    "F_pfpr_by_lm", "F_pfpr_by_rdt", "F_pfpr_by_pcr", "setup_XH_obj",
    "setup_XH_inits", "setup_XH_ix", "parse_XH_orbits", "get_XH_vars",
    "get_XH_pars", "change_XH_pars"
  )
  for (generic in generics) {
    expect_true(is.function(getS3method(generic, "trivialXH")))
    if (generic %in% c("setup_XH_obj", "get_skillset_XH")) {
      expect_true(is.function(getS3method(generic, "trivial")))
    } else {
      expect_null(getS3method(generic, "trivial", optional=TRUE))
    }
  }
  expect_identical(get_skillset_XH("trivial"), get_skillset_XH("trivialXH"))
  expect_true("make_XH_obj_trivial" %in% getNamespaceExports("ramp.xds"))
  expect_false("make_XH_obj_trivialXH" %in% getNamespaceExports("ramp.xds"))
})

test_that("trivial compatibility setup produces canonical objects and solutions", {
  args <- list(nPatches=2, membership=1:2, residence=1:2,
               HPop=c(100, 200), XHoptions=list(kappa=c(0.1, 0.2)))
  canonical <- do.call(xds_setup, c(list(XHname="trivialXH"), args))
  expect_no_warning(compat <- do.call(xds_setup, c(list(XHname="trivial"), args)))
  expect_identical(compat, canonical)
  expect_identical(compat$XHname, "trivialXH")
  expect_identical(class(compat$XH_obj[[1]]), "trivialXH")
  expect_identical(xds_solve(compat, Tmax=2), xds_solve(canonical, Tmax=2))

  canonical$nHostSpecies <- 2
  canonical$nStrata <- c(2, 2)
  for (i in 1:2) {
    options <- list(H=c(300, 400), kappa=c(0.3, 0.4))
    named <- setup_XH_obj("trivialXH", 1:2, canonical, options, i)
    expect_no_warning(aliased <- setup_XH_obj("trivial", 1:2, canonical, options, i))
    expect_identical(aliased, named)
    expect_identical(class(aliased$XH_obj[[i]]), "trivialXH")
    expect_equal(aliased$XH_obj[[i]]$H, options$H)
    expect_equal(aliased$XH_obj[[i]]$kappa, options$kappa)
  }
})

test_that("trivialXH preserves forcing and stateless behavior", {
  model <- xds_setup(
    XHname="trivialXH", nPatches=2, membership=1:2, residence=1:2,
    HPop=c(100, 200), XHoptions=list(kappa=c(0.1, 0.2))
  )
  y <- get_inits(model, flatten=TRUE)
  expect_identical(model$XHname, "trivialXH")
  expect_identical(get_skillset_XH(model), get_skillset_XH("trivialXH"))
  expect_equal(F_H(0, y, model, 1), c(100, 200))
  expect_equal(F_I(0, y, model, 1), c(10, 40))
  expect_equal(F_infectivity(y, model, 1), 1)
  expect_identical(check_XH(model, c(0, 2), 1), model)
  expect_identical(setup_XH_inits(model), model)
  expect_identical(setup_XH_ix(model, 1), model)
  expect_identical(dXHdt(0, y, model, 1), numeric())
  expect_identical(Update_XHt(0, y, model, 1), numeric())
  expect_identical(get_XH_vars(y, model), list())
  expect_identical(parse_XH_orbits(matrix(y, nrow=1), model, 1), list())

  changed <- change_XH_pars(model, list(kappa=c(0.3, 0.4)))
  expect_equal(F_I(0, y, changed, 1), c(30, 80))
  expect_equal(F_H(0, y, changed, 1), c(100, 200))
  expect_true(inherits(model$MY_obj[[1]], "SI"))
  expect_true(inherits(model$L_obj[[1]], "trivialL"))
  expect_true(is.list(get_skillset_MY("trivial")))
  expect_true(is.list(get_skillset_L("trivial")))
})

test_that("trivialXH supports host strata that differ from patch counts", {
  for (residence in list(c(1, 1, 2), 1)) {
    n <- length(residence)
    H <- 100*seq_len(n)
    kappa <- 0.1*seq_len(n)
    model <- xds_setup(
      XHname="trivialXH", nPatches=2, membership=1:2,
      residence=residence, HPop=H, XHoptions=list(kappa=kappa)
    )
    y <- get_inits(model, flatten=TRUE)
    expect_equal(model$XH_obj[[1]]$H, H)
    expect_equal(model$XH_obj[[1]]$kappa, kappa)
    expect_equal(F_H(0, y, model, 1), H)
    expect_equal(F_I(0, y, model, 1), H*kappa)

  }
})

test_that("trivialXH construction uses the selected host species strata", {
  model <- xds_setup(XHname="trivialXH", nPatches=2, residence=1:2)
  model$nHostSpecies <- 2
  model$nStrata <- c(2, 3)
  updated <- setup_XH_obj(
    "trivialXH", c(1, 1, 2), model, list(H=50, kappa=0.2), i=2
  )
  expect_equal(updated$XH_obj[[2]]$H, rep(50, 3))
  expect_equal(updated$XH_obj[[2]]$kappa, rep(0.2, 3))
  expect_identical(updated$XH_obj[[1]], model$XH_obj[[1]])
})

test_that("full and isolated setups solve with trivialXH", {
  models <- list(
    xds_setup(XHname="trivialXH", Lname="basicL"),
    xds_setup_mosy(),
    xds_setup_aquatic()
  )
  for (model in models) {
    expect_identical(model$XHname, "trivialXH")
    expect_identical(class(model$XH_obj[[1]]), "trivialXH")
    solved <- xds_solve(model, Tmax=2)
    expect_equal(solved$outputs$time, 0:2)
    expect_true(all(is.finite(solved$outputs$deout)))
  }
})
