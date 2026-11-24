test_that("SI infective biting returns a vector with survival-adjusted patch values", {
  fn <- getFromNamespace("F_fqZ.SI", "ramp.xds")
  for (npatches in c(1L, 3L)) {
    for (s in 1:2) {
      ix <- 3L + seq_len(npatches)
      y <- seq_len(3L + npatches)
      survival <- matrix(0.1, npatches, npatches)
      diag(survival) <- 0.6
      object <- list(ix = list(Y_ix = ix),
                     f = seq_len(npatches) * 0.2,
                     q = rep(0.7, npatches), Upsilon = survival)
      model <- list(nPatches = npatches, MY_obj = list(list(), list()))
      model$MY_obj[[s]] <- object
      expected <- vapply(seq_len(npatches), function(patch) {
        object$f[patch] * object$q[patch] *
          sum(survival[patch, ] * y[ix])
      }, numeric(1))
      result <- fn(0, y, model, s)
      expect_type(result, "double")
      expect_null(dim(result))
      expect_length(result, npatches)
      expect_equal(result, expected)
    }
  }
})
