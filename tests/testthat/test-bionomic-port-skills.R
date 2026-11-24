test_that("non-trivial MY skillsets identify the implemented bionomic ports", {
  parameters <- c("f", "q", "g", "sigma", "sigma_b", "sigma_q",
                  "mu", "nu", "eip", "K_matrix")
  ports <- list(
    basicM = c("f", "q", "g", "sigma", "mu", "nu", "K_matrix"),
    SI = c("f", "q", "g", "sigma", "mu", "nu", "eip", "K_matrix"),
    GeM = c("f", "q", "g", "sigma", "mu", "nu", "eip", "K_matrix"),
    macdonald = "K_matrix",
    RMdts = character()
  )
  for (module in names(ports)) {
    skills <- get_skillset_MY(module)
    expect_false("not_implemented" %in% names(skills))
    flags <- skills[grepl("_is_port$", names(skills))]
    expect_named(flags, paste0(parameters, "_is_port"))
    expect_identical(unname(unlist(flags)), parameters %in% ports[[module]],
                     info = module)
    bionomics <- getS3method("MBionomics", module)
    calls <- all.names(body(bionomics), functions = TRUE)
    expect_setequal(parameters[parameters %in% ports[[module]]],
                    parameters[paste0("F_", parameters) %in% calls])
    model <- list(MY_obj = list(structure(list(), class = "trivialMY"),
                                structure(list(), class = module)))
    expect_identical(get_skillset_MY(model, 2), skills)
    expect_true(skills$xde)
    expect_identical(skills$dts, module == "SI")
    expect_identical(skills$delay, module %in% c("GeM", "macdonald", "RMdts"))
  }
  expect_false(any(grepl("_is_port$", names(get_skillset_MY("trivialMY")))))
  expect_identical(get_skillset_MY("trivial"), get_skillset_MY("trivialMY"))
  expect_false("not_implemented" %in% names(get_skillset_MY("trivialMY")))
})

test_that("non-trivial L skillsets identify the implemented bionomic ports", {
  skills <- get_skillset_L("basicL")
  expect_false("not_implemented" %in% names(skills))
  flags <- skills[grepl("_is_port$", names(skills))]
  expect_identical(flags, list(psi_is_port = TRUE, phi_is_port = TRUE,
                               xi_is_port = TRUE, theta_is_port = TRUE))
  calls <- all.names(body(getS3method("LBionomics", "basicL")), functions = TRUE)
  expect_true(all(paste0("F_", c("psi", "phi", "xi", "theta")) %in% calls))
  model <- list(L_obj = list(structure(list(), class = "trivialL"),
                             structure(list(), class = "basicL")))
  expect_identical(get_skillset_L(model, 2), skills)
  expect_identical(skills[c("dts", "xde", "delay")],
                   list(dts = TRUE, xde = TRUE, delay = FALSE))
  expect_false(any(grepl("_is_port$", names(get_skillset_L("trivialL")))))
  expect_identical(get_skillset_L("trivial"), get_skillset_L("trivialL"))
  expect_false("not_implemented" %in% names(get_skillset_L("trivialL")))
})
