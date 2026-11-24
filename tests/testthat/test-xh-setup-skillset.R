test_that("XH setup methods do not call the skill-set getter", {
  namespace <- asNamespace("ramp.xds")
  methods <- ls(namespace, pattern = "^setup_XH_obj\\.")
  expect_length(methods, 4L)
  for (name in methods) {
    expect_false(
      "get_skillset_XH" %in% all.names(body(get(name, envir = namespace))),
      info = name
    )
  }
})
