test_that("XH skill sets share the SIS logical schema", {
  env <- new.env(parent = asNamespace("ramp.xds"))
  for (module in c("SIS", "hMoI", "trivialXH", "trivial")) {
    copy_package_function(paste0("get_skillset_XH.", module), env)
  }
  fields <- c("dts", "xde", "delay", "demography", "malaria",
              "pf_lm", "pf_rdt", "pf_pcr", "mda", "msat")
  for (module in c("SIS", "hMoI", "trivialXH", "trivial")) {
    getter <- get(paste0("get_skillset_XH.", module), env)
    skills <- getter(NULL, 1)
    expected_fields <- append(fields, "pr_obs", after = 5L)
    if (module %in% c("trivialXH", "trivial")) {
      expected_fields <- setdiff(expected_fields, c("pf_lm", "pf_rdt", "pf_pcr"))
    }
    expect_named(skills, expected_fields)
    expect_true(all(vapply(skills, function(x) {
      is.logical(x) && length(x) == 1L && !is.na(x)
    }, logical(1))))
    expect_identical(skills$dts, module == "SIS")
    expect_true(skills$xde)
    expect_false(skills$delay)
    expect_identical(skills$demography, module == "SIS")
    expect_identical(skills$malaria, module %in% c("SIS", "hMoI"))
    expect_true(skills$pr_obs)
    for (field in c("pf_lm", "pf_rdt", "pf_pcr")) {
      if (skills$malaria) {
        expect_false(skills[[field]], info = field)
      } else {
        expect_false(field %in% names(skills), info = field)
      }
    }
    for (field in c("mda", "msat")) {
      expect_identical(skills[[field]], module == "SIS", info = field)
    }
    model <- list(XH_obj = list(NULL, structure(list(), class = module)))
    expect_identical(getter(model, 2), skills)
  }
})
