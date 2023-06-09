test_that("cbc_lm works", {

  df <- data.frame(ids = rep(1:20, 5),
                   vals = stats::rnorm(100),
                   outs = stats::rnorm(25, 10, 100))

  (test <- lm(outs ~ vals, df))
  (res <- cbc_lm(data = df, formula = outs ~ vals, .case = "ids"))

  expect_equal(2 * 2, 4)
})
