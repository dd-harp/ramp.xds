test_that("importation and its ports do not require dependent variables", {
  model <- xds_setup()
  expect_identical(names(formals(Importation)), c("t", "xds_obj"))
  expect_identical(names(formals(importation_dynamics)), c("t", "xds_obj"))
  for (class in c("static", "dynamic")) {
    expect_identical(names(formals(getS3method("Importation", class))),
                     c("t", "xds_obj"))
    model$import_obj <- structure(list(), class = class)
    expect_identical(Importation(3, model), model)
  }
  for (port in c("update_visitors", "update_vis_kappa",
                 "update_time_away", "update_travel_eir")) {
    index <- if (port %in% c("update_visitors", "update_vis_kappa")) "s" else "i"
    update <- getExportedValue("ramp.xds", port)
    expect_identical(names(formals(update)), c("t", "xds_obj", index))
    expect_identical(names(formals(getS3method(port, "static"))),
                     c("t", "xds_obj", index))
    expect_identical(update(3, model, 1), model)
  }
})

test_that("dynamic importation preserves port updates for each host", {
  record <- function(t, xds_obj, species, port) {
    xds_obj$calls <- c(xds_obj$calls, list(
      list(t = t, species = species, port = port)
    ))
    xds_obj
  }
  local_mocked_bindings(
    update_visitors = function(t, xds_obj, s) record(t, xds_obj, s, "visitors"),
    update_vis_kappa = function(t, xds_obj, s) record(t, xds_obj, s, "vis_kappa"),
    update_time_away = function(t, xds_obj, i) record(t, xds_obj, i, "time_away"),
    update_travel_eir = function(t, xds_obj, i) record(t, xds_obj, i, "travel_eir"),
    .package = "ramp.xds"
  )
  model <- list(nHostSpecies = 2L, import_obj = structure(list(), class = "dynamic"))
  result <- Importation(3, model)
  expect_identical(vapply(result$calls, `[[`, "", "port"),
                   rep(c("visitors", "vis_kappa", "time_away", "travel_eir"), 2))
  expect_identical(vapply(result$calls, `[[`, 0L, "species"), rep(1:2, each = 4))
  expect_identical(vapply(result$calls, `[[`, 0, "t"), rep(3, 8))
})
