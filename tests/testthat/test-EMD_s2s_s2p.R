test_that("EMD of arbitrairy list works", {
  df <- data.frame(val=c(rep(1,6)), what=c(rep("sample-to-sample", 3), rep("sample-to-prediction", 3)))
  gd <- list("distances" = df)
  expect_equal(0, EMD_s2s_s2p(gd))
})
