test_that("discrete-time cohort outputs require at least two distinct rounded times", {
  xds_obj <- xds_setup_eir(xds = "dts")

  rounded_birthday <- xds_solve_cohort(xds_obj, birthday = 0.6)
  expect_equal(rounded_birthday$outputs$time[[1]], 1)
  expect_error(
    xds_solve_cohort(xds_obj, ages = c(0, 0.5)),
    "Invalid times vector"
  )
})

test_that("cohort ages use the time-grid utility for each solver", {
  xde_obj <- xds_setup_eir()
  dts_obj <- xds_setup_eir(xds = "dts")

  expect_equal(ramp.xds:::make_ages(xde_obj, Amax = 4, da = 2), c(0, 2, 4))
  expect_equal(ramp.xds:::make_ages(dts_obj, Amax = 4, da = 2), c(0, 2, 4))
  expect_equal(ramp.xds:::make_ages(dts_obj, ages = c(0, 3)), c(0, 3))
  expect_equal(
    ramp.xds:::make_ages(xde_obj, Amax = 2, da = 1) + 0.5,
    c(0.5, 1.5, 2.5)
  )
})

test_that("continuous-time cohort solving preserves fractional birthdays", {
  result <- xds_solve_cohort(
    xds_setup_eir(),
    birthday = 0.5,
    ages = c(0, 1)
  )

  expect_equal(unname(result$outputs$time), c(0.5, 1.5))
})

test_that("discrete-time cohort solving returns the requested calendar times", {
  xds_obj <- xds_setup_eir(xds = "dts")
  result <- xds_solve_cohort(xds_obj, birthday = 2, ages = c(0, 3))

  expect_equal(unname(result$outputs$time), c(2, 5))
  expect_equal(unname(result$outputs$deout[, 1]), c(2, 5))
})

test_that("discrete-time grids and requested times are rounded to integers", {
  xds_obj <- xds_setup_eir(xds = "dts")

  expect_equal(ramp.xds:::make_times_dts(Tmax = 1, dt = 0.5), c(0, 1))
  expect_equal(
    ramp.xds:::make_times_dts(times = c(0.2, 0.8, 2.4)),
    c(0, 1, 2)
  )

  result <- xds_solve(xds_obj, dt = 0.1, times = c(0, 0.3, 0.6, 0.9))
  expect_equal(unname(result$outputs$time), c(0, 1))
})
