test_that("frame checks forward times before each species index", {
  times <- c(-2, 0, 3)
  record_check <- function(xds_obj, component, times, species = NULL) {
    xds_obj$checks <- c(xds_obj$checks, list(
      list(component = component, times = times, species = species)
    ))
    xds_obj
  }
  local_mocked_bindings(
    check_XH = function(xds_obj, t=0, i) {
      record_check(xds_obj, "XH", t, i)
    },
    check_MY = function(xds_obj, t=0, s) {
      record_check(xds_obj, "MY", t, s)
    },
    check_L = function(xds_obj, t=0, s) {
      record_check(xds_obj, "L", t, s)
    },
    check_blood_feeding = function(xds_obj, t=0) {
      record_check(xds_obj, "blood_feeding", t)
    },
    .package = "ramp.xds"
  )
  routes <- list(
    full = c("XH", "XH", "MY", "L", "MY", "L", "blood_feeding"),
    mosy = c("MY", "L", "MY", "L", "blood_feeding"),
    aquatic = c("L", "MY", "L", "MY"),
    human = c("XH", "XH", "MY", "MY", "blood_feeding"),
    eir = "XH"
  )
  species <- list(
    full = list(1L, 2L, 1L, 1L, 2L, 2L, NULL),
    mosy = list(1L, 1L, 2L, 2L, NULL),
    aquatic = list(1L, 1L, 2L, 2L),
    human = list(1L, 2L, 1L, 2L, NULL),
    eir = list(1)
  )
  for (frame in names(routes)) {
    model <- list(frame = structure(frame, class = frame),
                  nHostSpecies = 2L, nVectorSpecies = 2L)
    checked <- check_models(model, t=times)
    expect_identical(vapply(checked$checks, `[[`, "", "component"), routes[[frame]])
    expect_identical(lapply(checked$checks, `[[`, "species"), species[[frame]])
    expect_true(all(vapply(checked$checks, function(check) {
      identical(check$times, times)
    }, logical(1))))
    default <- check_models(model)
    expect_true(all(vapply(default$checks, function(check) {
      identical(check$times, 0)
    }, logical(1))))
  }
})

test_that("component checks and methods accept times as their second argument", {
  times <- c(-2, 0, 3)
  registrations <- getNamespaceInfo("ramp.xds", "S3methods")
  for (generic in c("check_XH", "check_MY", "check_L")) {
    check <- getExportedValue("ramp.xds", generic)
    index <- if (generic == "check_XH") "i" else "s"
    expect_identical(names(formals(check)), c("xds_obj", "t", index))
    modules <- registrations[registrations[, 1] == generic, 2]
    for (module in modules) {
      method <- getS3method(generic, module)
      expect_identical(names(formals(method)), c("xds_obj", "t", index))
      component <- sub("^check_", "", generic)
      model <- setNames(list(list(NULL, structure(list(), class = module))),
                        paste0(component, "_obj"))
      expect_identical(check(model, times, 2), model)
      expect_identical(check(model, t=times, 2), model)
      expect_identical(do.call(check, c(list(model), setNames(list(2), index))), model)
    }
  }
  expect_identical(names(formals(check_blood_feeding)), c("xds_obj", "t"))
  expect_identical(check_blood_feeding(list(), t=times), list())
})
