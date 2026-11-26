test_that("visitor infectiousness is bounded between zero and one", {
  expect_no_error(check_vis_kappa(c(0, 0.5, 1), 3))
  expect_error(check_vis_kappa(c(0.5, 1 + 1e-8), 2))
  expect_error(check_vis_kappa(c(0.5, -1e-8), 2))
  expect_error(check_vis_kappa(c(0.5, Inf), 2))
  expect_error(check_vis_kappa(c(0.5, NA_real_), 2))
  expect_error(check_vis_kappa("0.5", 1))
  expect_error(check_vis_kappa(c(0, 1), 3))
})
