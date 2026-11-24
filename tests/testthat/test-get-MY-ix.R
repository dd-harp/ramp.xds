test_that("get_MY_ix defaults to the first vector species", {
  model <- list(MY_obj = list(
    list(ix = list(M_ix = 1:3)),
    list(ix = list(M_ix = 4:6))
  ))
  expect_identical(get_MY_ix(model), model$MY_obj[[1]]$ix)
  expect_identical(get_MY_ix(model, 2), model$MY_obj[[2]]$ix)
  expect_identical(get_MY_ix(model, s = 2), model$MY_obj[[2]]$ix)
})
