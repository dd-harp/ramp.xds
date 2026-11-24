test_that("F_pr_obs dispatches on the XH module object", {
  expect_identical(names(formals(F_pr_obs)), c("vars", "XH_obj"))
  vars <- list(m2 = c(0.2, 0.7))
  module <- make_XH_obj_hMoI(2)
  expect_equal(F_pr_obs(vars, module), 1 - exp(-vars$m2))
  expect_error(F_pr_obs(vars, structure(list(), class = "unsupported")),
               "no applicable method")
})

test_that("get_PR.obs uses saved orbits and the selected species", {
  module <- make_XH_obj_hMoI(2)
  model <- list(
    XH_obj = list(NULL, module),
    outputs = list(
      time = c(0, 1),
      orbits = list(XH = list(NULL, list(m2 = rbind(c(0.2, 0.7), c(0.4, 0.9))))))
  )
  original <- model
  expect_equal(get_PR(model, "obs", i = 2),
               list(time = c(0, 1), pr = 1 - exp(-rbind(c(0.2, 0.7), c(0.4, 0.9))),
                    method = "obs"))
  expect_identical(model, original)
})

test_that("trivialXH supports test positivity without inventing prevalence states", {
  object <- make_XH_obj_trivial(2, list())
  expect_identical(object$q, c(0.8, 0.8))
  expect_identical(make_XH_obj_trivial(2, list(), q = 0.6)$q, c(0.6, 0.6))
  expect_identical(make_XH_obj_trivial(2, list(q = c(0.2, 0.9)))$q, c(0.2, 0.9))
  expect_error(make_XH_obj_trivial(2, list(q = c(0.1, 0.2, 0.3))))
  model <- list(XH_obj = list(object, object),
                outputs = list(time = 0:1, orbits = list(XH = list(list(), list()))))
  expect_identical(get_XH_pars(model, 2)$q, object$q)
  expect_identical(change_XH_pars(model, list(), 2), model)
  changed <- change_XH_pars(model, list(q = c(0.4, 0.7)), 2)
  expect_identical(changed$XH_obj[[1]], object)
  expect_identical(changed$XH_obj[[2]]$q, c(0.4, 0.7))
  expect_identical(F_pr_obs(list(), object), numeric(0))
  expect_identical(get_PR(model, "obs", 2),
                   list(time = 0:1, pr = numeric(0), method = "obs"))
  expect_true(get_skillset_XH("trivialXH")$pr_obs)
  expect_true(get_skillset_XH("trivial")$pr_obs)
  expect_false("q" %in% names(make_XH_obj_hMoI(2)))
  expect_true(is.function(getS3method("F_pr_obs", "hMoI")))
})

test_that("hMoI observed prevalence and diagnostic aliases use apparent infections", {
  object <- make_XH_obj_hMoI(3)
  states <- list(m1 = c(0.5, 2, 4), m2 = c(0, 0.3, 1.5))
  original <- states
  expected <- 1 - exp(-states$m2)
  expect_equal(F_pr_obs(states, object), expected)
  expect_equal(F_prevalence(states, object), 1 - exp(-states$m1))
  for (diagnostic in list(F_pfpr_by_lm, F_pfpr_by_rdt, F_pfpr_by_pcr)) {
    expect_equal(diagnostic(states, object), expected)
  }
  trajectory <- lapply(states, function(x) rbind(x, x * 2))
  model <- list(XH_obj = list(make_XH_obj_SIS(3), object),
                outputs = list(time = 0:1, orbits = list(XH = list(NULL, trajectory))))
  original_model <- model
  expected <- 1 - exp(-trajectory$m2)
  for (method in c("obs", "lm", "rdt", "pcr")) {
    expect_equal(get_PR(model, method, 2),
                 list(time = 0:1, pr = expected, method = method))
  }
  expect_identical(states, original)
  expect_identical(model, original_model)
  expect_true(get_skillset_XH("hMoI")$pr_obs)
  expect_true(get_skillset_XH(model, 2)$pr_obs)
  expect_identical(get_skillset_XH("hMoI")[c("pf_lm", "pf_rdt", "pf_pcr")],
                   list(pf_lm = FALSE, pf_rdt = FALSE, pf_pcr = FALSE))
})
