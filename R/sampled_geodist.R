#' Do `CAST::plot_geodist` with a random sample
#'
#' Use this function like `CAST::plot_geodist` (only to the intended extent though), except that a random sample is taken before `plot_geodist` is executed. This makes the computation much faster.
#'
#' @param x sf object, training data locations
#' @param modeldomain sf object of the study area
#' @param samples size of the sample to be taken
#' @param cvfolds a list of CV folds
#' @param cv_method a list with row indices
#' @param stat 'density' or 'ecdf' for plot design
#' @param showPlot logical
#'
#' @return A `CAST::plot_geodist` list
#' @export
#'
#' @examples
#' \dontrun{
#' # load package and mask
#' library(emodis)
#' load(system.file("extdata", "data", "mask.Rdata", package="emodis"))
#' # load a single example file
#' coordsname <- paste0(sprintf("%03d", 1), "_coords.Rdata")
#' coords_file <- file.path(system.file("extdata",
#' "samples", package="emodis"), "clusterGapped", coordsname)
#' load(coords_file)
#' # traditional plot_geodist
#' system.time(CAST::plot_geodist(pts, mask))
#' # sampled plot_geodist
#' system.time(sampled_geodist(pts, mask, 50))
#' }
sampled_geodist <- function(x, modeldomain, samples, cvfolds = NA, cv_method=TRUE, stat = 'density', showPlot = TRUE) {

  row_numbers <- sample(1:nrow(x), samples)
  row_numbers <- sort(row_numbers)
  if (!("ID" %in% names(x))) {x$ID <- 1:nrow(x)}

  sampled_x <- x[row_numbers,]
  if(FALSE %in% unique(sf::st_is_valid(sf::st_as_sf(modeldomain)))) {
    print("making modeldomain valid")
    modeldomain=sf::st_make_valid(sf::st_as_sf(modeldomain))
  }
  sampled_modeldomain <- sf::st_sample(sf::st_as_sf(modeldomain), samples)
  sampled_modeldomain <- sf::st_transform(sampled_modeldomain, sf::st_crs(x))

  if(!is.na(cvfolds)[1]) {
    # this works because row_numbers is ordered
    l <- data.frame(row.names = 1:700)
    l[row_numbers,1] <- 1:10

    if(cv_method == "random" | cv_method == "spatial") {
      sampled_cvfolds <- lapply(cvfolds, function(x) {
        x <- l[x[x %in% row_numbers],]
      })
      gd <- CAST::plot_geodist(x = sampled_x, modeldomain = sampled_modeldomain, cvfolds = sampled_cvfolds, stat = stat, showPlot = showPlot)
    }
    if(cv_method == "nndm") {
      index_in <- cvfolds$idx_train[row_numbers]
      index_ex <- cvfolds$idx_test[row_numbers]
      index_in <- lapply(index_in, function(x) {
        x <- l[x[x %in% row_numbers],]
      })
      index_ex <- lapply(index_ex, function(x) {
        x <- l[x[x %in% row_numbers],]
      })
      gd <- CAST::plot_geodist(x = sampled_x, modeldomain = sampled_modeldomain, cvfolds = index_in, cvtrain = index_ex, stat = stat, showPlot = showPlot)
    }
  } else {
    gd <- CAST::plot_geodist(x = sampled_x, modeldomain = sampled_modeldomain, stat = stat, showPlot = showPlot)
  }
  return(gd)
}
