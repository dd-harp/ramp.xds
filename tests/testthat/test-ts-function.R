test_that("time-series setup preserves functions, parameters, and existing fields", {
  original <- structure(list(value = 7), class = "test_obj")
  supplied <- function(t, vars) t + 2
  for (function_name in c("F_season", "F_trend", "F_shock",
                          "H_season", "H_trend", "H_shock")) {
    parameter_name <- paste0(sub("^F_", "", function_name), "_par")
    parameters <- list(name = "F_one")
    options <- setNames(list(supplied, parameters),
                        c(function_name, parameter_name))
    result <- ramp.xds:::make_ts_obj(original, options)
    expect_identical(result[[function_name]], supplied)
    expect_identical(result[[parameter_name]], list())
    expect_identical(result$value, original$value)
    expect_identical(class(result), class(original))

    options[[function_name]] <- list()
    result <- ramp.xds:::make_ts_obj(original, options)
    expect_identical(result[[parameter_name]], parameters)
    expect_equal(result[[function_name]](0, list()), 1)
    expect_identical(original, structure(list(value = 7), class = "test_obj"))
  }
})

test_that("XH time-series constructor resolves option overrides for both patterns", {
  supplied <- function(t, vars) 3
  for (function_name in c("F_season", "F_trend", "F_shock",
                          "H_season", "H_trend", "H_shock")) {
    parameter_name <- paste0(sub("^F_", "", function_name), "_par")
    options <- setNames(list(supplied), function_name)
    object <- make_XH_obj_trivial(2, options)
    expect_identical(object[[function_name]], supplied)
    expect_identical(object[[parameter_name]], list())
    arguments <- c(list(nPatches = 2, options = list()), options)
    object <- do.call(make_XH_obj_trivial, arguments)
    expect_identical(object[[function_name]], supplied)
    expect_identical(object[[parameter_name]], list())
  }
})
