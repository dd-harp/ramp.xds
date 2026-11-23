test_that("get_MY_ix defaults to the first vector species", {
  definitions <- parse(test_path("..", "..", "R", "adult-MY-interface.R"))
  definition <- Filter(function(expr) {
    is.call(expr) && as.character(expr[[1]])[1] %in% c("<-", "=") &&
      identical(expr[[2]], as.name("get_MY_ix"))
  }, definitions)
  expect_length(definition, 1L)
  env <- new.env(parent = baseenv())
  eval(definition[[1]], env)
  model <- list(MY_obj = list(
    list(ix = list(M_ix = 1:3)),
    list(ix = list(M_ix = 4:6))
  ))
  expect_identical(env$get_MY_ix(model), model$MY_obj[[1]]$ix)
  expect_identical(env$get_MY_ix(model, 2), model$MY_obj[[2]]$ix)
  expect_identical(env$get_MY_ix(model, s = 2), model$MY_obj[[2]]$ix)
})
