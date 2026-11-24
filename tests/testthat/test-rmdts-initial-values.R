test_that("RMdts initial-value changes use stored values and species-specific cohort sizes", {
  env <- new.env(parent = asNamespace("ramp.xds"))
  for (name in c("make_MY_inits_RMdts", "change_MY_inits.RMdts")) {
    copy_package_function(name, env)
  }
  env$checkIt <- ramp.xds::checkIt
  for (nPatches in c(2L, 5L)) {
    cohorts <- c(3L, 7L)
    model <- list(nPatches = nPatches, MY_obj = lapply(cohorts, function(max_eip) {
      list(max_eip = max_eip, inits = env$make_MY_inits_RMdts(nPatches, max_eip))
    }))
    for (s in seq_along(cohorts)) {
      expect_identical(env$change_MY_inits.RMdts(model, s = s), model)
      for (state in names(model$MY_obj[[s]]$inits)) {
        value <- seq_along(model$MY_obj[[s]]$inits[[state]]) + 10
        expected <- model
        expected$MY_obj[[s]]$inits[[state]] <- value
        expect_identical(
          env$change_MY_inits.RMdts(model, setNames(list(value), state), s), expected
        )
      }
      result <- env$change_MY_inits.RMdts(model, list(Y = 2), s)
      expect_identical(result$MY_obj[[s]]$inits$Y, rep(2, nPatches * cohorts[s]))
      expect_identical(result$MY_obj[[3L - s]], model$MY_obj[[3L - s]])
      expect_error(env$change_MY_inits.RMdts(model, list(Y = c(1, 2)), s))
      expect_error(env$change_MY_inits.RMdts(model, list(Y = "invalid"), s))
      options <- lapply(model$MY_obj[[s]]$inits, function(value) value + 1)
      expected <- model
      expected$MY_obj[[s]]$inits <- options
      expect_identical(env$change_MY_inits.RMdts(model, options, s), expected)
    }
  }
})
