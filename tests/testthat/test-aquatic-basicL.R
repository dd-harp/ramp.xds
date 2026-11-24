test_that("basicL steady states satisfy the differential equations", {
  for (nHabitats in c(1, 3)) {
    for (xi in c(0, 0.01)) {
      model <- xds_setup_aquatic(nHabitats=nHabitats, Loptions=list(xi=xi))
      eta <- seq_len(nHabitats)
      equilibrium <- steady_state_L(eta, model)
      expect_named(equilibrium, "L")
      expect_length(equilibrium$L, nHabitats)
      expect_true(all(is.finite(equilibrium$L)))
      expect_true(all(equilibrium$L >= 0))

      y <- get_inits(model, flatten=TRUE)
      y[model$L_obj[[1]]$ix$L_ix] <- equilibrium$L
      model$terms$eta[[1]] <- eta
      residual <- dLdt(0, y, model, s=1)
      expect_lt(max(abs(residual)), 1e-5)
    }
  }
})

test_that("basicL discrete steady states satisfy the one-day update", {
  for (nHabitats in c(1, 3)) {
    for (xi in c(0, 0.01)) {
      model <- xds_setup_aquatic(nHabitats=nHabitats, Loptions=list(xi=xi))
      model$xds <- structure("dts", class="dts")
      eta <- seq_len(nHabitats)
      equilibrium <- steady_state_L(eta, model)
      expect_identical(equilibrium, steady_state_L_basicL.dts(eta, model))
      expect_named(equilibrium, "L")
      expect_length(equilibrium$L, nHabitats)
      expect_true(all(is.finite(equilibrium$L)))
      expect_true(all(equilibrium$L >= 0))

      y <- get_inits(model, flatten=TRUE)
      y[model$L_obj[[1]]$ix$L_ix] <- equilibrium$L
      model$terms$eta[[1]] <- eta
      residual <- Update_Lt(0, y, model, s=1) - equilibrium$L
      expect_lt(max(abs(residual)), 1e-5)
    }
  }
})

test_that("basicL parameter getters return evaluated values for the selected species", {
  getters <- list(psi=get_psi, phi=get_phi, xi=get_xi, theta=get_theta)
  for (nHabitats in c(1, 3)) {
    model <- xds_setup_aquatic(nHabitats=nHabitats)
    model$L_obj[[2]] <- model$L_obj[[1]]
    for (parameter in names(getters)) {
      value <- seq_len(nHabitats)/10
      model$L_obj[[2]][[parameter]] <- value
      model$L_obj[[2]][[paste0(parameter, "_t")]] <- value+1
      original <- model

      expect_identical(getters[[parameter]](model),
                       get_L_pars(model, s=1)[[parameter]])
      expect_identical(getters[[parameter]](model, s=1),
                       get_L_pars(model, s=1)[[parameter]])
      expect_identical(getters[[parameter]](model, s=2), value)
      expect_identical(model, original)
    }
  }
})

test_that("basicL parameter changes update ports and persist after evaluation", {
  for (nHabitats in c(1, 3)) {
    model <- xds_setup_aquatic(nHabitats=nHabitats)
    model$L_obj[[2]] <- model$L_obj[[1]]
    original <- model
    options <- list(psi=0.2, xi=0.01, phi=0.15, theta=0.02)

    changed <- change_L_pars(model, options, s=2)
    expect_identical(changed$L_obj[[1]], original$L_obj[[1]])
    expect_identical(model, original)

    for (parameter in names(options)) {
      expected <- rep(options[[parameter]], nHabitats)
      module <- changed$L_obj[[2]]
      expect_equal(module[[paste0(parameter, "_obj")]][[parameter]], expected)
      expect_identical(class(module[[paste0(parameter, "_obj")]]), "static")
      expect_equal(module[[paste0(parameter, "_t")]], expected)
      expect_identical(module[[parameter]], original$L_obj[[2]][[parameter]])
    }

    y <- get_inits(changed, flatten=TRUE)
    evaluated <- LBionomics(1, y, changed, s=2)
    evaluated <- LEffectSizes(1, y, evaluated, s=2)
    for (parameter in names(options)) {
      expect_equal(get_L_pars(evaluated, s=2)[[parameter]],
                   rep(options[[parameter]], nHabitats))
    }

    partial <- change_L_pars(changed, list(psi=0.3), s=2)
    expect_equal(partial$L_obj[[2]]$psi_t, rep(0.3, nHabitats))
    expect_equal(partial$L_obj[[2]]$psi_obj$psi, rep(0.3, nHabitats))
    expect_identical(get_L_pars(partial, s=2), get_L_pars(changed, s=2))
    expect_identical(change_L_pars(changed, s=2), changed)
  }
})

test_that("individual aquatic parameter changers update their corresponding ports", {
  changers <- list(psi=change_psi, phi=change_phi, xi=change_xi, theta=change_theta)
  model <- xds_setup_aquatic(nHabitats=3)
  model$L_obj[[2]] <- model$L_obj[[1]]

  for (parameter in names(changers)) {
    value <- c(0.01, 0.02, 0.03)
    changed <- changers[[parameter]](value, model, s=2)
    module <- changed$L_obj[[2]]
    expect_identical(module[[paste0(parameter, "_obj")]][[parameter]], value)
    expect_identical(module[[paste0(parameter, "_t")]], value)
    expect_identical(module[[parameter]], value)
    expect_null(module$f_obj)
    expect_identical(changed$L_obj[[1]], model$L_obj[[1]])

    y <- get_inits(changed, flatten=TRUE)
    evaluated <- LBionomics(1, y, changed, s=2)
    evaluated <- LEffectSizes(1, y, evaluated, s=2)
    expect_identical(get_L_pars(evaluated, s=2)[[parameter]], value)
  }
})
