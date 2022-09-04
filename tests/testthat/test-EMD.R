test_that("EMD of arbitrairy list works", {
  df <- data.frame(dist=seq(1,6,1), dist1=rep(1,6), dist2=rep(1,6))
  expect_equal(0, EMD(df, "dist1", "dist2"))
})
