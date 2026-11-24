test_that("basicM retrieves initial mosquito density for the selected species", {
  for (M in list(5, c(patch1=5, patch2=10))) {
    model <- list(MY_obj=list(
      structure(list(inits=list(M=M, P=M/2)), class=c("basicM", "basicM_xde")),
      structure(list(inits=list(M=M+2, P=M/2)), class="basicM")
    ))
    original <- model

    expect_identical(get_M(model), M)
    expect_identical(get_M(model, s=1), M)
    expect_identical(get_M(model, s=2), M+2)
    expect_identical(model, original)
  }
})
