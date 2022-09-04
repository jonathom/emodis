#' EMD between sample-to-prediction and CV-distances
#'
#' Calculates the Earth Mover's distance between the sample-to-prediction and the CV-distances
#' nearest neighbour distance distributions of a given `CAST::plot_geodist` output.
#'
#' @param df A dataframe, output of `emodis::discrete_curve()`
#' @param dist1 The name of the first distribution
#' @param dist2 The name of the second distributions
#'
#' @description dist1 and dist2 can be "sample-to-sample", "sample-to-prediction" or "CV-distances"
#'
#' @return The EMD
#' @export
#'
#' @examples
EMD <- function(df, dist1, dist2) {
  emdist::emdw(A=df[,dist1], wA=rep(1,length(df)), B=df[,dist2], wB=rep(1,length(df)), dist = "euclidean")
}
