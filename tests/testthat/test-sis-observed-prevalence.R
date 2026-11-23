test_that("SIS uses one test-positivity parameter", {
  default <- make_XH_obj_SIS(3)
  expect_identical(default$q, rep(0.8, 3))
  expect_false(any(c("q_lm", "q_rdt", "q_pcr") %in% names(default)))
  expect_false(any(c("q_lm", "q_rdt", "q_pcr") %in%
                     names(formals(make_XH_obj_SIS))))
  expect_identical(make_XH_obj_SIS(3, q = 0.6)$q, rep(0.6, 3))
  expect_identical(make_XH_obj_SIS(3, options = list(q = c(0.2, 0.5, 0.9)))$q,
                   c(0.2, 0.5, 0.9))
  expect_error(make_XH_obj_SIS(3, q = c(0.1, 0.2)))

  model <- list(XH_obj = list(default, make_XH_obj_SIS(3, q = 0.4)))
  pars <- get_XH_pars(model, 2)
  expect_named(pars, c("b", "c", "r", "rho", "sigma", "q"))
  expect_identical(pars$q, rep(0.4, 3))
  expect_identical(change_XH_pars(model, list(), 2), model)
  changed <- change_XH_pars(model, list(q = c(0.1, 0.3, 0.7)), 2)
  expect_identical(changed$XH_obj[[1]], model$XH_obj[[1]])
  expect_identical(get_XH_pars(changed, 2)$q, c(0.1, 0.3, 0.7))
  expect_identical(get_XH_pars(changed, 2)[-6], pars[-6])
})

test_that("SIS diagnostic aliases all use observed prevalence", {
  module <- make_XH_obj_SIS(2, q = c(0.5, 0.8))
  states <- list(I = c(10, 40), H = c(100, 200))
  expected <- c(0.05, 0.16)
  expect_equal(F_pr_obs(states, module), expected)
  for (generic in list(F_pfpr_by_lm, F_pfpr_by_rdt, F_pfpr_by_pcr)) {
    expect_equal(generic(states, module), expected)
  }
  trajectory <- list(I = matrix(c(10, 20, 40, 60), nrow = 2, byrow = TRUE),
                     H = matrix(c(100, 200, 100, 200), nrow = 2, byrow = TRUE))
  model <- list(
    XH_obj = list(NULL, module),
    outputs = list(time = c(0, 1), orbits = list(XH = list(NULL, trajectory)))
  )
  original <- model
  expected <- F_pr_obs(trajectory, module)
  expect_equal(dim(expected), c(2L, 2L))
  for (method in c("obs", "lm", "rdt", "pcr")) {
    expect_equal(get_PR(model, method, 2),
                 list(time = c(0, 1), pr = expected, method = method))
  }
  expect_identical(model, original)
  expect_true(get_skillset_XH("SIS")$pr_obs)
  for (field in c("pf_lm", "pf_rdt", "pf_pcr")) {
    expect_false(get_skillset_XH("SIS")[[field]])
  }
})
