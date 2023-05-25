# test_that("multiplication works", {
#   df <- data.frame(id = c(1,2,3,4,5),
#                    var1 = c(3,7,4,5,8),
#                    var2 = c(7,3,9,4,7),
#                    var3 = c(8,5,3,9,7),
#                    var4 = c(1,5,3,9,6))
#
#   olstraj_out <- OLStraj(data = df,
#                          varlist = c("var1", "var2", "var3", "var4"),
#                          regtype = "both")
#
#   expect_equal(2 * 2, 4)
# })
