test_that("RMdts exposes static parameters and initial densities", {
  module <- make_MY_obj_macdonald(2)
  class(module) <- "RMdts"
  module$K_matrix <- matrix(0, 2, 2)
  module$inits <- make_MY_inits_RMdts(2, 3)
  model <- list(MY_obj=list(module, module))

  pars <- get_MY_pars(model)
  expect_equal(pars$f, get_f(model))
  expect_equal(pars$q, get_q(model))
  expect_equal(pars$g, get_g(model))
  expect_equal(pars$sigma, get_sigma(model))
  expect_equal(get_M(model), module$inits$M)
  expect_equal(get_Z(model), module$inits$Z)

  expect_identical(change_MY_pars(model), model)
  changed <- change_MY_pars(model, list(f=c(0.2, 0.4), nu=c(2, 3)), 2)
  expect_identical(changed$MY_obj[[1]], module)
  expect_equal(get_f(changed, 2), c(0.2, 0.4))
  expect_equal(get_MY_pars(changed, 2)$nu, c(2, 3))
  expect_equal(get_q(changed, 2), pars$q)
  expect_equal(get_g(changed, 2), pars$g)
  expect_equal(get_sigma(changed, 2), pars$sigma)
  expect_equal(get_M(changed, 2), module$inits$M)
  expect_equal(get_Z(changed, 2), module$inits$Z)

  expect_identical(MBionomics(0, numeric(), model, 2), model)
  expect_identical(MEffectSizes(0, numeric(), model, 2), model)
})
