test_that("random habitat search weights use habitat count rather than patch count", {
  for (membership in list(c(1, 1, 2), c(1, 2))) {
    model <- xds_setup(nPatches = 2, membership = membership)
    original <- model
    generator <- function(n, p1, p2) seq_len(n)
    changed <- setup_habitat_search_weights(
      "random", model, options = list(f_rand = generator)
    )
    expected <- seq_along(membership)
    expect_equal(get_habitat_search_weights(changed), expected)
    expect_length(get_habitat_search_weights(changed), length(membership))
    expect_equal(changed$terms$Q[[1]],
                 as.vector(get_habitat_matrix(model) %*% expected))
    expect_equal(changed$habitats$laying_matrix[[1]],
                 make_U_matrix(expected, get_habitat_matrix(model),
                               changed$terms$Q[[1]]))
    expect_identical(model, original)
  }
})
