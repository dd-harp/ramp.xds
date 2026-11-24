test_that("MY and L changers refresh bionomics without applying effect sizes", {
  namespace <- asNamespace("ramp.xds")
  methods <- ls(namespace, pattern = "^change_(MY|L)_pars\\.")
  count <- length(methods)
  for (name in methods) {
      component <- if (startsWith(name, "change_MY")) "MY" else "L"
      env <- new.env(parent = namespace)
      copy_package_function(name, env)
      parameters <- c(
        "f", "q", "g", "sigma", "mu", "nu", "eip", "eggsPerBatch", "eggs_per_batch",
        "psi", "phi", "xi", "theta", "Lambda", "Z", "eggs"
      )
      object <- setNames(as.list(rep(1, length(parameters))), parameters)
      for (parameter in parameters) {
        object[[paste0(parameter, "_obj")]] <- setNames(list(1), parameter)
      }
      for (fun in c("F_season", "F_trend", "F_shock")) object[[fun]] <- function(t) 1
      model <- list(nHabitats = 1L, MY_obj = list(object, object), L_obj = list(object, object))
      env$get_inits <- function(xds_obj, flatten) {
        expect_true(flatten)
        c(state = 7)
      }
      env$checkIt <- function(value, n) value
      env$change_Omega <- function(xds_obj, s) xds_obj
      calls <- 0L
      env[[paste0(substr(component, 1, 1), "Bionomics")]] <- function(t, y, xds_obj, s) {
        calls <<- calls + 1L
        expect_identical(t, 0)
        expect_identical(y, c(state = 7))
        expect_identical(s, 2L)
        expect_identical(xds_obj[[paste0(component, "_obj")]][[s]][[parameters[1]]], 2)
        xds_obj$refreshed <- TRUE
        xds_obj
      }
      env$MEffectSizes <- env$LEffectSizes <- function(...) stop("Unexpected effect-size call")
      parameters <- if (component == "MY") "f" else "Lambda"
      if (name == "change_L_pars.basicL") parameters <- "psi"
      if (parameters == "psi") {
        # This method updates a port rather than the effective value.
        original <- env$LBionomics
        env$LBionomics <- function(t, y, xds_obj, s) {
          expect_identical(xds_obj$L_obj[[s]]$psi_obj$psi, 2)
          xds_obj$L_obj[[s]]$psi <- 2
          original(t, y, xds_obj, s)
        }
      }
      if (component == "MY" && !name %in% c("change_MY_pars.macdonald", "change_MY_pars.RMdts", "change_MY_pars.trivialMY")) {
        original <- env$MBionomics
        env$MBionomics <- function(t, y, xds_obj, s) {
          expect_identical(xds_obj$MY_obj[[s]]$f_obj$f, 2)
          xds_obj$MY_obj[[s]]$f <- 2
          original(t, y, xds_obj, s)
        }
      }
      result <- env[[name]](model, setNames(list(2), parameters), 2L)
      expect_true(result$refreshed, info = name)
      expect_identical(calls, 1L, info = name)
      expect_identical(result[[paste0(component, "_obj")]][[1]], object)
  }
  expect_identical(count, 8L)
})
