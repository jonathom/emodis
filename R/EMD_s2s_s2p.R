#' EMD between sample-to-sample and sample-to-prediction
#'
#' Calculates the Earth Mover's distance between the sample-to-sample and the sample-to-prediction
#' nearest neighbour distance distributions of a given `CAST::plot_geodist` output.
#'
#' @param geodist The output of a `CAST:plot_geodist()` call
#'
#' @return A numeric value.
#' @export
#'
#' @examples
EMD_s2s_s2p <- function(geodist) {
  df <- geodist$distances
  dist1 <- df[df$what=="sample-to-sample",1]
  dist2 <- df[df$what=="sample-to-prediction",1]
  emdist::emdw(A=dist1, wA=rep(1,length(dist1)), B=dist2, wB=rep(1,length(dist2)), dist = "euclidean")
}
