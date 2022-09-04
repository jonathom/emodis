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
#' \dontrun{
#' library(emodis)
#' load(system.file("extdata", "data", "mask.Rdata", package="emodis"))
#' # load a single example file
#' coordsname <- paste0(sprintf("%03d", 1), "_coords.Rdata")
#' coords_file <- file.path(system.file("extdata", "samples", package="emodis"), "clusterGapped", coordsname)
#' load(coords_file)
#' dist <- CAST::plot_geodist(pts, mask)
#' dist_discrete <- discrete_curve(dist)
#' # calculate distance between the curves
#' EMD(dist_discrete, "sample-to-sample", "sample-to-prediction")
#' }
EMD <- function(df, dist1, dist2) {
  emdist::emdw(A=df[,dist1], wA=rep(1,length(df)), B=df[,dist2], wB=rep(1,length(df)), dist = "euclidean")
}
