test_that("type 2 emigration uses the sugar shape parameter in both sugar factors", {
  for (nPatches in c(1, 3)) {
    sugar <- seq_len(nPatches)
    model <- list(
      nPatches=nPatches,
      patches=list(sugar=list(rep(0, nPatches), sugar)),
      terms=list(B=list(rep(0, nPatches), rep(0, nPatches)),
                 Qall=list(rep(0, nPatches), rep(0, nPatches))),
      MY_obj=list(setup_sigma_obj(rep(0.1, nPatches), list()),
                  list(sigma_obj=make_F_sigma_type2(
                    nPatches, sigX=0.3, sigB=0.2, sigQ=0.4, sigS=0.7)))
    )
    expected <- 0.3*0.7*sugar/(1+0.7*sugar)
    expect_equal(F_sigma(0, model, s=2), expected)
    expect_length(F_sigma(0, model, s=2), nPatches)
    expect_identical(F_sigma(0, model, s=1), rep(0.1, nPatches))

    model$MY_obj[[2]]$sigma_obj$sigB <- 0.9
    expect_equal(F_sigma(0, model, s=2), expected)

    blood <- seq_len(nPatches)/2
    water <- seq_len(nPatches)/3
    model$terms$B[[2]] <- blood
    model$terms$Qall[[2]] <- water
    expected <- 0.3*(0.4*water/(1+0.4*water) +
                     0.9*blood/(1+0.9*blood) +
                     0.7*sugar/(1+0.7*sugar))
    expect_equal(F_sigma(0, model, s=2), expected)

    model$patches$sugar[[2]] <- rep(0, nPatches)
    expected <- 0.3*(0.4*water/(1+0.4*water) +
                     0.9*blood/(1+0.9*blood))
    expect_equal(F_sigma(0, model, s=2), expected)
  }
})
