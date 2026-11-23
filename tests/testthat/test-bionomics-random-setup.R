test_that("random bionomics draws once and dispatches to static values", {
  parameters <- c("f", "q", "g", "sigma", "mu", "nu", "eip")
  for (parameter in parameters) {
    constructor <- get(paste0("make_F_", parameter, "_random"))
    setup <- get(paste0("setup_F_", parameter))
    initialize <- get(paste0("setup_", parameter, "_obj"))
    evaluate <- get(paste0("F_", parameter))
    port_name <- paste0(parameter, "_obj")
    for (nPatches in c(1, 3)) {
      calls <- 0L
      generator <- function(n, p1, p2) {
        calls <<- calls+1L
        expect_identical(n, nPatches)
        expect_identical(p1, 0.1)
        expect_identical(p2, 0.2)
        seq_len(n)*p1+p2
      }
      options <- list(f_rand=generator, p1=0.1, p2=0.2)
      expected <- seq_len(nPatches)*0.1+0.2
      config <- constructor(nPatches, options)
      expect_identical(calls, 1L)
      expect_identical(class(config), "static")
      expect_equal(config[[parameter]], expected)
      expect_length(config[[parameter]], nPatches)
      expect_identical(config$f_rand, generator)
      expect_identical(config$p1, 0.1)
      expect_identical(config$p2, 0.2)

      direct <- constructor(nPatches, f_rand=generator, p1=0.1, p2=0.2)
      expect_identical(direct, config)
      expect_identical(calls, 2L)

      model <- make_xds_object_template(nPatches=nPatches,
                                       membership=seq_len(nPatches),
                                       residence=seq_len(nPatches))
      model$MY_obj <- list(initialize(rep(0.2, nPatches), list()),
                           initialize(rep(0.4, nPatches), list()))
      if (parameter=="eip") {
        model$MY_obj[[2]]$eip_obj$dF_eip <- function(t) 7
      }
      original <- model
      result <- setup("random", model, options, s=2)
      expect_identical(calls, 3L)
      expect_identical(result$MY_obj[[2]][[port_name]], config)
      expect_identical(result$MY_obj[[1]], model$MY_obj[[1]])
      for (t in c(0, 10, 100)) {
        expect_equal(evaluate(t, result, s=2), expected)
        expect_identical(evaluate(t, result, s=1), rep(0.2, nPatches))
      }
      expect_identical(calls, 3L)
      expect_identical(model, original)
      expect_identical(setup(c(list(name="random"), options), model, s=2), result)
      expect_identical(calls, 4L)
      if (parameter=="eip") {
        expect_identical(dF_eip(10, result, s=2), 0)
        expect_identical(result$MY_obj[[2]]$eip_obj$dF_eip(10), 0)
      }

      calls_before <- calls
      broken_generator <- function(n, p1, p2) stop("generator failed")
      expect_error(constructor(nPatches, list(f_rand=broken_generator)),
                   "generator failed")
      expect_identical(calls, calls_before)
    }
  }
})

test_that("default random bionomics uses reproducible setup-time draws", {
  defaults <- list(f=c(280, 1000), q=c(960, 1000), g=c(80, 1000),
                   sigma=c(100, 1000), mu=c(8, 1000), nu=c(980, 1000),
                   eip=c(12, 1))
  for (parameter in names(defaults)) {
    constructor <- get(paste0("make_F_", parameter, "_random"))
    evaluate <- get(paste0("F_", parameter))
    distribution <- if (parameter=="eip") stats::rgamma else stats::rbeta
    for (nPatches in c(1, 3)) {
      set.seed(123)
      expected <- distribution(nPatches, defaults[[parameter]][1],
                               defaults[[parameter]][2])
      set.seed(123)
      port <- constructor(nPatches)
      expect_identical(port[[parameter]], expected)
      model <- list(MY_obj=list(setNames(list(port), paste0(parameter, "_obj"))))
      seed <- .Random.seed
      expect_identical(evaluate(0, model, 1), expected)
      expect_identical(evaluate(10, model, 1), expected)
      expect_identical(.Random.seed, seed)
    }
  }
})

test_that("MBionomics reuses all random setup values without drawing again", {
  local_mocked_bindings(
    F_K_matrix=function(t, xds_obj, s) diag(xds_obj$nPatches),
    .package="ramp.xds"
  )
  parameters <- c("f", "q", "g", "sigma", "mu", "nu", "eip")
  nPatches <- 3
  model <- make_xds_object_template(nPatches=nPatches,
                                   membership=seq_len(nPatches),
                                   residence=seq_len(nPatches))
  adult <- list()
  for (parameter in parameters) {
    initialize <- get(paste0("setup_", parameter, "_obj"))
    adult <- initialize(rep(0.2, nPatches), adult)
  }
  class(adult) <- "SI"
  model$MY_obj <- list(adult)
  calls <- 0L
  generator <- function(n, p1, p2) {
    calls <<- calls+1L
    seq_len(n)*p1+p2
  }
  expected <- seq_len(nPatches)*0.1+0.2
  for (parameter in parameters) {
    setup <- get(paste0("setup_F_", parameter))
    model <- setup("random", model, list(f_rand=generator, p1=0.1, p2=0.2))
  }
  expect_identical(calls, 7L)
  for (t in c(0, 10, 100)) {
    model <- MBionomics(t, numeric(), model, s=1)
    for (parameter in parameters) {
      value_name <- paste0(parameter, "_t")
      expect_equal(model$MY_obj[[1]][[value_name]], expected)
    }
  }
  expect_identical(calls, 7L)
})
