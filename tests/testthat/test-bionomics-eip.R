test_that("static EIP derivatives are zero rather than EIP values", {
  for (nPatches in c(1, 3)) {
    model <- make_xds_object_template(nPatches=nPatches, membership=seq_len(nPatches),
                                     residence=seq_len(nPatches))
    model$MY_obj <- list(setup_eip_obj(rep(12, nPatches), list()),
                         setup_eip_obj(seq_len(nPatches)+15, list()))
    original <- model
    for (s in c(1, 2)) {
      for (t in c(0, 10)) {
        expect_identical(dF_eip(t, model, s), 0)
        expect_identical(dF_eip(t, model, s), dF_eip.static(t, model, s))
        expect_identical(F_eip(t, model, s), model$MY_obj[[s]]$eip_obj$eip)
      }
    }
    expect_identical(model, original)
  }
})

test_that("EIP derivative dispatch forwards time, model and species", {
  local_mocked_bindings(
    dF_eip.static=function(t, xds_obj, s) list(t=t, eip=xds_obj$MY_obj[[s]]$eip_obj$eip, s=s),
    .package="ramp.xds"
  )
  model <- make_xds_object_template()
  model$MY_obj <- list(setup_eip_obj(12, list()), setup_eip_obj(18, list()))
  expect_identical(dF_eip(5, model, s=2), list(t=5, eip=18, s=2))
})
