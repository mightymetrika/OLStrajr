# test_that("cbc_lm works", {
#
#   df <- data.frame(ids = rep(1:5, 5),
#                    vals = stats::rnorm(25),
#                    outs = stats::rnorm(25, 10, 25))
#
#   res <- cbc_lm(data = df, formula = "outs ~ vals", .case = "ids")
#   expect_equal(2 * 2, 4)
# })
