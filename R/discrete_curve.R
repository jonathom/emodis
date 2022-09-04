#' Iterate through result folders to obtain a dataframe
#'
#' Discretizes the dataframe output by `plot_geodist()`.
#'
#' @param gd The output of a `plot_geodist()` call
#' @param resolution numerical How many bins should the result have?
#'
#' @return A dataframe
#' @export
#'
#' @examples
discrete_curve <- function(gd, resolution=100) {
  sts <- gd$distances$dist[gd$distances$what == "sample-to-sample"]
  stp <- gd$distances$dist[gd$distances$what == "sample-to-prediction"]
  if ("CV-distances" %in% levels(gd$distances$what)) {
    cv  <- gd$distances$dist[gd$distances$what == "CV-distances"]
  } else {
    cv <- max(stp)
  }

  lower <- min(min(sts), min(stp), min(cv))
  upper <- max(max(sts), max(stp), max(cv))

  distance <- upper - lower
  step <- distance / (resolution - 1)
  # step*100 == distance

  l_sts <- length(sts)
  l_stp <- length(stp)
  l_cv  <- length(cv)

  fg <- data.frame(matrix(ncol=4, nrow=resolution))

  dists <- seq(from=lower, to=upper, by=step)
  for (i in 1:resolution) {
    fg[i,1] <- i # assign distance
    fg[i,2] <- length(sts[sts > dists[i] & sts <= dists[i+1]]) / l_sts
    fg[i,3] <- length(stp[stp > dists[i] & stp <= dists[i+1]]) / l_stp
    if ("CV-distances" %in% levels(gd$distances$what)) {
      fg[i,4] <- length(cv[cv > dists[i] & cv <= dists[i+1]]) / l_cv
    } else {
      fg[i,4] <- NA
    }
  }
  # fg <- na.omit(fg)

  names(fg) <- c("dist", "sample-to-sample", "sample-to-prediction", "CV-distances")

  return(fg)
}
