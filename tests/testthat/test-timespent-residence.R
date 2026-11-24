test_that("default time spent setup uses the explicitly supplied residence", {
  model <- list(nPatches = 3, nStrata = c(2, 3),
                residence = c(1, 1),
                XH_obj = list(list(), list()))
  first <- c(1, 3)
  model <- setup_timespent("setup", model, list(residence = first), i = 1)
  expect_identical(get_residence(model, 1), first)
  expect_equal(get_timespent_matrix(model, 1), make_residence_matrix(3, first))
  expect_null(model$residence)
  first_object <- model$XH_obj[[1]]

  second <- c(2, 1, 2)
  changed <- setup_timespent("setup", model, list(residence = second), i = 2)
  expect_identical(get_residence(changed, 2), second)
  expect_equal(get_timespent_matrix(changed, 2), make_residence_matrix(3, second))
  expect_identical(changed$XH_obj[[1]], first_object)
  expect_identical(class(changed$XH_obj[[2]]$timespent_obj), "static")
  expect_null(changed$residence)
  expect_identical(model$XH_obj[[2]], list())
})
