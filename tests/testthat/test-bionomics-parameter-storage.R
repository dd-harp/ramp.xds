test_that("MY ports separate baselines from realized parameters", {
  for (module in c("basicM", "GeM", "SI", "macdonald")) {
    env <- new.env(parent = asNamespace("ramp.xds"))
    constructor <- if (module == "basicM") "make_M_obj_basicM" else paste0("make_MY_obj_", module)
    copy_package_function(constructor, env)
    copy_package_function(paste0("MBionomics.", module), env)
    copy_package_function(paste0("MEffectSizes.", module), env)
    copy_package_function("change_K_matrix.K", env)
    ports <- if (module == "macdonald") character() else c("f", "q", "g", "sigma", "mu", "nu")
    if (module %in% c("GeM", "SI")) ports <- c(ports, "eip")
    ports <- c(ports, "K_matrix")
    for (npatches in c(1L, 3L)) {
      for (s in 1:2) {
        object <- env[[constructor]](npatches)
        object$Upsilon_obj <- structure(list(), class = if (module == "basicM") "na" else "dde")
        object$unported <- rep(9, npatches)
        model <- list(nPatches = npatches, xds = structure("xde", class = "xde"),
                      MY_obj = list(object, object))
        K <- matrix(0, npatches, npatches)
        if (npatches > 1L) {
          K[] <- 1 / (npatches - 1L)
          diag(K) <- -1
        }
        model <- env$change_K_matrix.K(K, model, s = s)
        original <- model$MY_obj[[s]]
        for (par in setdiff(ports, "K_matrix")) {
          env[[paste0("F_", par)]] <- local({
            parameter <- par
            function(t, xds_obj, s) {
              xds_obj$MY_obj[[s]][[paste0(parameter, "_obj")]][[parameter]] + t * 0.01
            }
          })
        }
        baseline <- env[[paste0("MBionomics.", module)]](2, numeric(), model, s)
        expect_identical(baseline$MY_obj[[3L - s]], model$MY_obj[[3L - s]])
        neutral <- env[[paste0("MEffectSizes.", module)]](2, numeric(), baseline, s)
        for (par in ports) {
          expect_equal(neutral$MY_obj[[s]][[par]], baseline$MY_obj[[s]][[paste0(par, "_t")]])
        }
        for (par in ports) {
          expected <- if (par == "K_matrix") K else original[[paste0(par, "_obj")]][[par]] + 0.02
          expect_equal(baseline$MY_obj[[s]][[paste0(par, "_t")]], expected, info = par)
          expect_identical(baseline$MY_obj[[s]][[par]], original[[par]])
          expect_true(all(baseline$MY_obj[[s]][[paste0("es_", par)]] == 1))
          baseline$MY_obj[[s]][[paste0("es_", par)]] <-
            if (par == "K_matrix") matrix(0, npatches, npatches) else rep(0.5, npatches)
        }
        result <- env[[paste0("MEffectSizes.", module)]](2, numeric(), baseline, s)
        expect_identical(result$MY_obj[[3L - s]], model$MY_obj[[3L - s]])
        for (par in ports) {
          expect_equal(result$MY_obj[[s]][[par]],
                       baseline$MY_obj[[s]][[paste0(par, "_t")]] *
                         baseline$MY_obj[[s]][[paste0("es_", par)]], info = par)
        }
        expect_identical(result$MY_obj[[s]]$unported, original$unported)
        if (module == "macdonald") {
          for (par in c("f", "q", "g", "sigma", "mu", "nu", "eip")) {
            expect_identical(result$MY_obj[[s]][[par]], original[[par]])
          }
        }
        realized <- result$MY_obj[[s]]
        expect_equal(realized$Omega,
                     ramp.xds::F_Omega_xde(realized$g, realized$sigma, realized$mu, realized$K_matrix))
        if (module != "basicM") {
          expect_equal(realized$Upsilon, ramp.xds::F_Upsilon_xde(realized$eip, realized$Omega))
        }
        repeated <- env[[paste0("MEffectSizes.", module)]](2, numeric(), result, s)
        expect_identical(repeated, result)
        reset <- env[[paste0("MBionomics.", module)]](3, numeric(), result, s)
        for (par in ports) expect_true(all(reset$MY_obj[[s]][[paste0("es_", par)]] == 1))
      }
    }
  }
})

test_that("basicL ports apply effects only in LEffectSizes", {
  env <- new.env(parent = asNamespace("ramp.xds"))
  for (name in c("make_L_obj_basicL", "LBionomics.basicL", "LEffectSizes.basicL")) {
    copy_package_function(name, env)
  }
  for (nhabitats in c(1L, 3L)) {
    for (s in 1:2) {
      object <- env$make_L_obj_basicL(nhabitats)
      model <- list(nHabitats = nhabitats, L_obj = list(object, object))
      baseline <- env$LBionomics.basicL(0, numeric(), model, s)
      for (par in c("psi", "phi", "xi", "theta")) {
        expect_identical(baseline$L_obj[[s]][[par]], object[[par]])
        expect_equal(baseline$L_obj[[s]][[paste0(par, "_t")]], object[[paste0(par, "_obj")]][[par]])
        baseline$L_obj[[s]][[paste0("es_", par)]] <- rep(0.5, nhabitats)
      }
      result <- env$LEffectSizes.basicL(0, numeric(), baseline, s)
      for (par in c("psi", "phi", "xi", "theta")) {
        expect_equal(result$L_obj[[s]][[par]], object[[par]] * 0.5)
      }
      expect_identical(result$L_obj[[3L - s]], object)
      expect_identical(env$LEffectSizes.basicL(0, numeric(), result, s), result)
    }
  }
})
