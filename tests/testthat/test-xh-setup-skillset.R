test_that("XH setup methods do not call the skill-set getter", {
  count <- 0L
  for (file in list.files(test_path("..", "..", "R"), "\\.R$", full.names = TRUE)) {
    for (definition in parse(file)) {
      if (!is.call(definition) ||
          !as.character(definition[[1]])[1] %in% c("<-", "=") ||
          !grepl("^setup_XH_obj\\.", as.character(definition[[2]])[1])) next
      expect_false("get_skillset_XH" %in% all.names(definition[[3]]),
                   info = as.character(definition[[2]]))
      count <- count + 1L
    }
  }
  expect_identical(count, 4L)
})
