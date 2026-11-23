test_that("RMdts index setup and consumers use module-level storage", {
  env <- new.env(parent = baseenv())
  env$tail <- utils::tail
  for (definition in parse(test_path("..", "..", "R", "adult-MY-RMdts.R"))) {
    if (is.call(definition) && as.character(definition[[1]])[1] %in% c("<-", "=")) {
      eval(definition, env)
    }
  }
  for (s in 1:2) {
    object <- list(max_eip = 4L, f = 2, q = 0.5, nu = 3, eggsPerBatch = 10)
    model <- list(max_ix = 7L, nPatches = 3L, MY_obj = list(object, object))
    result <- env$setup_MY_ix.RMdts(model, s)
    ix <- result$MY_obj[[s]]$ix
    expect_identical(names(ix), c("M_ix", "P_ix", "U_ix", "Y_ix", "Z_ix"))
    expect_equal(lengths(ix), c(M_ix = 3, P_ix = 3, U_ix = 3, Y_ix = 12, Z_ix = 3))
    expect_equal(unlist(ix, use.names = FALSE), 8:31)
    expect_equal(result$max_ix, 31)
    expect_null(result$ix)
    expect_identical(result$MY_obj[[3L - s]], object)
    y <- seq_len(result$max_ix)
    vars <- env$get_MY_vars.RMdts(y, result, s)
    expect_identical(vars$M, y[ix$M_ix])
    expect_identical(vars$Y, matrix(y[ix$Y_ix], 3, 4))
    expect_equal(env$F_fqZ.RMdts(0, y, result, s), y[ix$Z_ix])
    expect_equal(env$F_fqM.RMdts(0, y, result, s), y[ix$M_ix])
    expect_equal(env$F_eggs.RMdts(0, y, result, s), 30 * y[ix$M_ix])
    outputs <- rbind(y, y + 1)
    parsed <- env$parse_MY_orbits.RMdts(outputs, result, s)
    expect_identical(parsed$M, outputs[, ix$M_ix])
    expected_Y <- outputs[, ix$Y_ix[1:3]] + outputs[, ix$Y_ix[4:6]] +
      outputs[, ix$Y_ix[7:9]] + outputs[, ix$Y_ix[10:12]]
    expect_identical(parsed$Y, expected_Y)
  }
})

test_that("RMdts output parsing sums cohorts separately within each patch", {
  env <- new.env(parent = baseenv())
  env$tail <- utils::tail
  for (definition in parse(test_path("..", "..", "R", "adult-MY-RMdts.R"))) {
    if (is.call(definition) && as.character(definition[[1]])[1] %in% c("<-", "=")) {
      eval(definition, env)
    }
  }
  for (npatches in c(1L, 3L)) {
    for (ncohorts in c(1L, 4L)) {
      for (s in 1:2) {
        model <- list(max_ix = 5L, nPatches = npatches,
                      MY_obj = list(list(max_eip = 2L), list(max_eip = 3L)))
        model$MY_obj[[s]]$max_eip <- ncohorts
        model <- env$setup_MY_ix.RMdts(model, s)
        ix <- model$MY_obj[[s]]$ix
        for (ntimes in c(1L, 4L)) {
          outputs <- matrix(seq_len(ntimes * model$max_ix), nrow = ntimes)
          expected_Y <- matrix(0, nrow = ntimes, ncol = npatches)
          for (time in seq_len(ntimes)) {
            cohorts <- matrix(outputs[time, ix$Y_ix], nrow = npatches)
            expected_Y[time, ] <- rowSums(cohorts)
          }
          expected_Y <- drop(expected_Y)
          parsed <- env$parse_MY_orbits.RMdts(outputs, model, s)
          expect_identical(names(parsed), c("M", "P", "U", "Y", "Z", "y", "z", "parous"))
          expect_equal(parsed$Y, expected_Y)
          expect_identical(dim(parsed$Y), dim(parsed$M))
          expect_equal(parsed$y, expected_Y / outputs[, ix$M_ix])
          expect_identical(parsed$M, outputs[, ix$M_ix])
          expect_identical(parsed$P, outputs[, ix$P_ix])
          expect_identical(parsed$U, outputs[, ix$U_ix])
          expect_identical(parsed$Z, outputs[, ix$Z_ix])
          expect_equal(parsed$z, parsed$Z / parsed$M)
          expect_equal(parsed$parous, parsed$P / parsed$M)
        }
      }
    }
  }
})
