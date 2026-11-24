test_that("XH skill sets can be queried after setup", {
  for (module in c("SIS", "hMoI", "trivialXH", "trivial")) {
    model <- xds_setup(XHname=module, nPatches=2, membership=1:2,
                       residence=1:2)
    expect_false("skill_set" %in% names(model$XH_obj[[1]]), info = module)
    skills <- get_skillset_XH(model, 1)
    expect_type(skills, "list")
    expect_identical(skills, get_skillset_XH(module), info = module)
  }
})
