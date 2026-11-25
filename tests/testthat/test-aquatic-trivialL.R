test_that("trivialL constructors preserve supplied forcing functions", {
  functions <- list(
    function(t, vars) 2,
    function(t, vars) { value <- 3; value }
  )
  patterns <- c(F_season="season_par", F_trend="trend_par", F_shock="shock_par")

  for (pattern in names(patterns)) {
    parameter <- patterns[[pattern]]
    for (forcing in functions) {
      options <- setNames(list(forcing, list(name="F_one")), c(pattern, parameter))
      from_options <- make_L_obj_trivial(2, options)
      from_arguments <- do.call(make_L_obj_trivial,
                                c(list(nHabitats=2), options))
      for (module in list(from_options, from_arguments)) {
        expect_identical(module[[pattern]], forcing)
        expect_identical(module[[parameter]], list())
        model <- list(L_obj=list(module))
        expect_equal(F_emerge(0, numeric(), model, 1),
                     rep(100 * forcing(0, list()), 2))
      }
    }

    parameters <- list(name="F_one")
    module <- make_L_obj_trivial(2, setNames(list(parameters), parameter))
    expect_identical(module[[parameter]], parameters)
    expect_equal(module[[pattern]](0, list()), 1)
  }

  model <- list(L_obj=list(make_L_obj_trivial(2)))
  expect_equal(F_emerge(0, numeric(), model, 1), c(100, 100))
})

test_that("trivialL retains its constructor and only two legacy dispatch methods", {
  module <- make_L_obj_trivial(2, list(Lambda=c(10, 20)))
  expect_identical(class(module), "trivialL")
  expect_equal(module$Lambda, c(10, 20))
  expect_true("make_L_obj_trivial" %in% getNamespaceExports("ramp.xds"))
  expect_false("make_L_obj_trivialL" %in% getNamespaceExports("ramp.xds"))
  generics <- c(
    "get_skillset_L", "check_L", "dLdt", "Update_Lt", "F_emerge",
    "LBionomics", "LEffectSizes", "setup_L_obj", "get_L_pars",
    "change_L_pars", "setup_L_inits", "get_L_vars", "change_L_inits",
    "setup_L_ix", "parse_L_orbits"
  )
  for (generic in generics) {
    expect_true(is.function(getS3method(generic, "trivialL")))
    if (generic %in% c("setup_L_obj", "get_skillset_L")) {
      expect_true(is.function(getS3method(generic, "trivial")))
    } else {
      expect_null(getS3method(generic, "trivial", optional=TRUE))
    }
  }
  expect_identical(get_skillset_L("trivial"), get_skillset_L("trivialL"))
  expect_identical(get_skillset_L("trivialL"),
                   list(dts=FALSE, xde=TRUE, delay=FALSE))
})

test_that("trivialL setup aliases preserve forcing and stateless behavior", {
  args <- list(nPatches=2, membership=1:2, residence=1:2,
               Loptions=list(Lambda=c(10, 20)))
  model <- do.call(xds_setup, c(list(Lname="trivialL"), args))
  expect_no_warning(compat <- do.call(xds_setup, c(list(Lname="trivial"), args)))
  expect_identical(compat, model)
  expect_identical(model$Lname, "trivialL")
  expect_identical(get_skillset_L(model), get_skillset_L("trivialL"))
  expect_identical(xds_solve(compat, Tmax=2), xds_solve(model, Tmax=2))

  y <- get_inits(model, flatten=TRUE)
  expect_equal(F_emerge(0, y, model, 1), c(10, 20))
  expect_equal(get_L_pars(model)$Lambda, c(10, 20))
  changed <- change_L_pars(model, list(
    Lambda=c(30, 40), F_season=function(t, vars) 2,
    F_trend=function(t, vars) 3, F_shock=function(t, vars) 0.5
  ))
  expect_equal(F_emerge(0, y, changed, 1), c(90, 120))
  expect_identical(get_L_vars(y, model, 1), list())
  expect_identical(dLdt(0, y, model, 1), numeric())
  expect_identical(Update_Lt(0, y, model, 1), numeric())
  expect_identical(LBionomics(0, y, model, 1), model)
  expect_identical(LEffectSizes(0, y, model, 1), model)
  expect_identical(check_L(model, c(0, 2), 1), model)
  initialized <- setup_L_inits(model)
  expect_identical(initialized$L_obj[[1]]$inits, list())
  expect_identical(setup_L_inits(initialized), initialized)
  expect_identical(change_L_inits(model), model)
  expect_identical(setup_L_ix(model, 1), model)
  expect_identical(parse_L_orbits(matrix(y, nrow=1), model, 1), list())
  expect_identical(class(model$forced_by), "Lambda")

  template <- make_xds_object_template(nPatches=2, membership=c(1, 1, 2), residence=1:2)
  for (s in 1:2) {
    options <- list(Lambda=c(30, 50, 70))
    named <- setup_L_obj("trivialL", c(1, 1, 2), template, options, s)
    expect_no_warning(aliased <- setup_L_obj("trivial", c(1, 1, 2), template, options, s))
    expect_identical(aliased, named)
    expect_identical(class(aliased$L_obj[[s]]), "trivialL")
    expect_equal(aliased$L_obj[[s]]$Lambda, options$Lambda)
    expect_equal(aliased$nHabitats, 3)
    expect_identical(get_skillset_L(aliased, s), get_skillset_L("trivialL"))
    template <- named
  }
})

test_that("default and isolated setups use trivialL without changing other modules", {
  for (model in list(xds_setup(), xds_setup_mosy(Lname="trivialL"),
                    xds_setup_human(), xds_setup_eir())) {
    expect_identical(model$Lname, "trivialL")
    expect_identical(class(model$L_obj[[1]]), "trivialL")
    solved <- xds_solve(model, Tmax=2)
    expect_equal(solved$outputs$time, 0:2)
    expect_true(all(is.finite(solved$outputs$deout)))
  }
  model <- xds_setup(XHname="trivialXH", MYname="trivialMY")
  expect_identical(class(model$XH_obj[[1]]), "trivialXH")
  expect_identical(class(model$MY_obj[[1]]), "trivialMY")
  expect_identical(get_skillset_XH("trivial"), get_skillset_XH("trivialXH"))
  expect_identical(get_skillset_MY("trivial"), get_skillset_MY("trivialMY"))
  expect_identical(xds_setup_aquatic()$Lname, "basicL")
  expect_identical(xds_setup_mosy()$Lname, "basicL")
})
