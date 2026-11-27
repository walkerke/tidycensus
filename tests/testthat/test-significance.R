test_that("clevel sets the critical value; published MOEs are converted with 1.645", {
  # SE = MOE / 1.645 for published (90 percent) MOEs, so the test value is
  # 20 / sqrt(2 * (10 / 1.645)^2) = 2.326: significant at 90 and 95 percent,
  # not at 99 percent (critical value 2.576)
  expect_true(significance(100, 80, 10, 10, clevel = 0.9))
  expect_true(significance(100, 80, 10, 10, clevel = 0.95))
  expect_false(significance(100, 80, 10, 10, clevel = 0.99))
})

test_that("moe_level gives the confidence level of MOEs from get_acs(moe_level = )", {
  moe99 <- 10 * 2.576 / 1.645
  expect_false(significance(100, 80, moe99, moe99, clevel = 0.99, moe_level = 99))
  expect_error(significance(100, 80, 10, 10, moe_level = 80), "moe_level")
})
