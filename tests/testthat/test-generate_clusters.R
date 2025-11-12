test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("N with basic default values", {
  expect_equal(generate_clusters(J = 100,
                                 njrange = c(50, 100),
                                 seed = 123)$N,
               7495)
})

