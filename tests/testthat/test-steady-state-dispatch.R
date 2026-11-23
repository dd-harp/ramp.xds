test_that("steady-state dispatch selects the module and then xds", {
  local_mocked_bindings(
    steady_state_L_basicL.xde=function(eta, xds_obj, s=1) list(route="L-xde", s=s),
    steady_state_L_basicL.dts=function(eta, xds_obj, s=1) list(route="L-dts", s=s),
    steady_state_M_basicM.xde=function(Lambda, xds_obj, s=1) list(route="M-xde", s=s),
    steady_state_M_basicM.dts=function(Lambda, xds_obj, s=1) list(route="M-dts", s=s),
    steady_state_MY_SI.xde=function(Lambda, kappa, xds_obj, s=1) list(route="SI-xde", s=s),
    .package="ramp.xds"
  )
  model <- make_xds_object_template()
  model$L_obj <- list(structure(list(), class="unused"),
                      structure(list(), class=c("basicL", "basicL_dts")))
  model$MY_obj <- list(structure(list(), class="unused"),
                       structure(list(), class=c("basicM", "basicM_dts")))

  expect_identical(steady_state_L(1, model, s=2), list(route="L-xde", s=2))
  expect_identical(steady_state_M(1, model, s=2), list(route="M-xde", s=2))
  model$xds <- structure("dts", class="dts")
  expect_identical(steady_state_M(1, model, s=2), list(route="M-dts", s=2))
  expect_identical(steady_state_L(1, model, s=2), list(route="L-dts", s=2))

  model$MY_obj[[2]] <- structure(list(), class="SI")
  expect_error(steady_state_MY(1, 0.1, model, s=2),
               "no applicable method.*steady_state_MY_SI")
  model$xds <- structure("xde", class="xde")
  model$xde <- structure("dde", class="dde")
  expect_identical(steady_state_MY(1, 0.1, model, s=2), list(route="SI-xde", s=2))
  expect_identical(steady_state_L(1, model, s=2), list(route="L-xde", s=2))

  model$xds <- structure("unsupported", class="unsupported")
  expect_error(steady_state_MY(1, 0.1, model, s=2),
               "no applicable method.*steady_state_MY_SI")
  expect_error(steady_state_L(1, model, s=2),
               "no applicable method.*steady_state_L_basicL")
  model$MY_obj[[2]] <- structure(list(), class="basicM")
  expect_error(steady_state_M(1, model, s=2), "no applicable method.*steady_state_M_basicM")
})

test_that("differential steady states preserve formulas and shapes", {
  for (nPatches in c(1, 2)) {
    model <- make_xds_object_template(nPatches=nPatches, membership=seq_len(nPatches),
                                     residence=seq_len(nPatches))
    module <- list(nPatches=nPatches, f=rep(0.3, nPatches), q=rep(0.7, nPatches),
                   Omega=diag(0.2, nPatches),
                   ix=list(M_ix=seq_len(nPatches),
                           P_ix=nPatches+seq_len(nPatches),
                           Y_ix=2*nPatches+seq_len(nPatches)))
    model$MY_obj <- list(structure(module, class="basicM"))
    Lambda <- seq_len(nPatches)
    equilibrium <- steady_state_M(Lambda, model)
    expect_identical(equilibrium, steady_state_M_basicM.xde(Lambda, model))
    expect_equal(unname(equilibrium), c(Lambda/0.2, 0.3/(0.3+0.2)*Lambda/0.2))
    expected_names <- if (nPatches==1) c("M", "P") else
      c(paste0("M", seq_len(nPatches)), paste0("P", seq_len(nPatches)))
    expect_identical(names(equilibrium), expected_names)
    model$terms$Lambda <- list(Lambda)
    expect_lt(max(abs(dMYdt(0, unname(equilibrium), model, 1))), 1e-12)

    class(model$MY_obj[[1]]) <- "SI"
    kappa <- rep(0.1, nPatches)
    model$terms$kappa <- list(kappa)
    equilibrium <- steady_state_MY(Lambda, kappa, model)
    expect_identical(equilibrium, steady_state_MY_SI.xde(Lambda, kappa, model))
    expect_named(equilibrium, c("M", "P", "Y"))
    expect_lt(max(abs(dMYdt(0, unlist(equilibrium), model, 1))), 1e-12)
  }

  model <- xds_setup_aquatic()
  expect_identical(steady_state_L(1, model), steady_state_L_basicL.xde(1, model))
})

test_that("basicM discrete steady states satisfy the update in each species", {
  for (nPatches in c(1, 2, 3)) {
    model <- make_xds_object_template(nPatches=nPatches, membership=seq_len(nPatches),
                                     residence=seq_len(nPatches))
    model$xds <- structure("dts", class="dts")
    Omega <- diag(0.2, nPatches)
    if (nPatches > 1) {
      K <- matrix(1/(nPatches-1), nPatches, nPatches)
      diag(K) <- -1
      Omega <- Omega - K %*% diag(seq_len(nPatches)*0.05, nPatches)
    }
    module <- structure(list(nPatches=nPatches, f=seq_len(nPatches)*0.3,
                             Omega=Omega,
                             ix=list(M_ix=seq_len(nPatches),
                                     P_ix=nPatches+seq_len(nPatches))),
                        class="basicM")
    model$MY_obj <- list(module, module)
    model$MY_obj[[2]]$f <- module$f*0.8
    Lambda <- seq_len(nPatches)
    model$terms$Lambda <- list(Lambda, Lambda)
    original <- model
    expected_names <- if (nPatches==1) c("M", "P") else
      c(paste0("M", seq_len(nPatches)), paste0("P", seq_len(nPatches)))

    for (s in c(1, 2)) {
      equilibrium <- steady_state_M(Lambda, model, s=s)
      expect_identical(equilibrium, steady_state_M_basicM.dts(Lambda, model, s=s))
      expect_identical(names(equilibrium), expected_names)
      expect_length(equilibrium, 2*nPatches)
      expect_true(all(is.finite(equilibrium)))
      expect_true(all(equilibrium >= 0))
      residual <- Update_MYt(0, unname(equilibrium), model, s=s) - unname(equilibrium)
      expect_lt(max(abs(residual)), 1e-6)

      S <- expm::expm(-Omega)
      B <- diag(model$MY_obj[[s]]$f, nPatches)
      M <- as.vector(solve(diag(nPatches)-S, Lambda))
      P <- as.vector(solve(diag(nPatches)+B-S, B %*% M))
      expect_lt(max(abs(unname(equilibrium)-c(M, P))), 1e-5)
    }
    expect_identical(steady_state_M(Lambda, model), steady_state_M(Lambda, model, s=1))
    expect_identical(model, original)
  }
})
