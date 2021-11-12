# # Kamprath method #########
# expect_equal(
#   .lr_ka(c(-1:2, NA)), 
#   c(-1:2, NA) * 1.5
# )
# 
# expect_equal(
#   .lr_ka(c(-1:2, NA), lf = 2), 
#   c(-1:2, NA) * 2
# )
# 
# # Cochrane method #####
# expect_equal(
#   .lr_co(exch_ac = c(2, 4, 6, NA), ECEC = 10, TAS = 20),
#   c(0, 3, 8, NA)
# )
# 
# expect_equal(
#   .lr_co(exch_ac = c(2, 4, 6, NA), ECEC = 10, TAS = c(5, NA, 15, 20)),
#   c(3, NA, 9, NA),
# )
# 
# # NuMaSS method #######
# expect_equal(
#   .lr_nu(exch_ac = c(.3,.6), ECEC = 1, TAS = 20),
#   c(.32, .84)
# )
# 
# expect_equal(
#   .lr_nu(exch_ac = c(.3,.6), ECEC = 1, TAS = 20, clay = 0.01),
#   c(.32, .84)
# )
# 
# expect_equal(
#   .lr_nu(exch_ac = c(.3,.6), ECEC = 1, TAS = 20, clay = c(0.01, NA)),
#   c(.32, .84)
# )
# 
# 
# expect_equal(
#   .lr_nu(exch_ac = c(.3,.6), ECEC = 1, TAS = 20, clay = c(0.01, .99)),
#   c(.32, .84 * 25/13)
# )
# 
# expect_equal(
#   .lr_nu(exch_ac = c(.3,NA), ECEC = 1, TAS = 20, clay = c(NA, .5)),
#   c(.32, NA)
# )
# 
# 