#' EMD between sample-to-sample and CV-distances
#'
#' Calculates the Earth Mover's distance between the sample-to-sample and the CV-distances
#' nearest neighbour distance distributions of a given `CAST::plot_geodist` output.
#'
#' @param geodist
#'
#' @return
#' @export
#'
#' @examples
EMD_s2s_cv <- function(geodist) {
  df <- geodist$distances
  dist1 <- df[df$what=="sample-to-sample",1]
  dist2 <- df[df$what=="CV-distances",1]
  emdist::emdw(A=dist1, wA=rep(1,length(dist1)), B=dist2, wB=rep(1,length(dist2)), dist = "euclidean")
}
