#' Iterate through result folders to summarize them
#'
#' Obtains either a dataframe with results such as RMSE per realization and
#' method and EMD measures, or a list containing the same information but also
#' the `plot_geodist` objects for plotting purposes.
#'
#' @param results_root The folder containing the method subfolders
#' @param samples_root The folder containing the sample realizations
#' @param sample_designs A vector containing all sample designs that should be analyzed. Must be the same as in the file names.
#' @param samples Number of samples taken for sampled_geodist
#' @param return_EMD boolean. Whether to calculate the Earth Mover's distances between the distributions
#' @param methods vector of names of the CV methods. Should be the names of the folders contained in /results_root
#' @param iterations numeric. Through how many sampling designs should the function iterate?
#' @param return_type either "df" or "geodist". "df" returns the dataframe of values, "geodist" returns a list of all geodist objects that were calculated
#' @param mask_ modeldomain for plot_geodist
#'
#' @return A dataframe or a list (see return_type)
#' @export
#'
#' @examples
#' \dontrun{
#' # load packages
#' library(emodis)
#' library(ggplot2)
#' # load mask
#' load(system.file("extdata", "data", "mask.Rdata", package="emodis"))
#' # run evaluate
#' df <- evaluate_results(results_root = system.file("extdata", "CVresults", package="emodis"),
#' samples_root = system.file("extdata", "samples", package="emodis"),
#' return_EMD = TRUE, # to obtain EMD measures
#' iterations = 1, # to iterate through all 100 sampling realizations
#' mask_ = mask)
#' # make a plot
#' ggplot(data=df, aes(x=s2s_s2p, y=RMSE_val, color=sample)) +
#' geom_point()
#' }
evaluate_results <- function(results_root, samples_root,
                             sample_designs = c("simpleRandom", "regular", "clusterMedium", "clusterStrong", "clusterGapped"),
                             samples = 50, return_EMD = FALSE,
                             methods = c("random", "spatial", "nndm"),
                             iterations = 1,
                             return_type = "df",
                             mask_) {

  pts <- folds <- NULL

  df <- data.frame(method=character(), sample=character(), iteration=numeric(),
                   RMSE=numeric(), RMSE_val=numeric(), s2s_s2p=numeric(),
                   s2s_cv=numeric(), s2p_cv=numeric())

  geodist_list <- list()
  x <- 1

  for (smpl in sample_designs) {
    # nr_of_files <- length(list.files(file.path(results_root, method), glob2rx("AGB_*.Rdata")))
    for (method in methods) {
      for (iteration in 1:iterations){ # 1:nr_of_files) {
        # print(paste(method, smpl, iteration, "starting"))

        # define file names
        filename <- paste0("AGB_", smpl, sprintf("%03d", iteration), ".Rdata")
        result_file <- file.path(results_root, method, filename)
        exhaustive_file <- file.path(results_root, "exhaustive", filename)

        # define coordinate file
        coordsname <- paste0(sprintf("%03d", iteration), "_coords.Rdata")
        coords_file <- file.path(samples_root, smpl, coordsname)

        if (!file.exists(exhaustive_file)) next
        if (!file.exists(result_file)) next
        if (!file.exists(coords_file)) next

        # load the validation error of the specific sampling design
        load(exhaustive_file) # this loads "RMSE"
        RMSE_val <- RMSE

        # load the error estimated via CV
        load(result_file) # this loads and overwrites RMSE
        if(length(RMSE) > 1) {RMSE <- mean(RMSE)}

        # load sample coordinate and check if they're ok
        load(coords_file)
        if(class(pts)[1] != "sf") {
          pts_df <- as.data.frame(pts)
          pts_sf <- sf::st_as_sf(pts_df, coords = c("x", "y"))
          sf::st_crs(pts_sf) <- sf::st_crs("EPSG:3035") # set crs EPSG:3035
        } else {
          pts_sf <- pts
        }
        mask_ <- sf::st_transform(sf::st_as_sf(mask_), sf::st_crs(pts_sf))

        # just take the first iteration
        i <- 1
        if (method == "spatial") {
          fold_index <- i*2 - 1
          flds_i <- folds[fold_index][[1]]
        }
        if (method == "random") {
          fold_indices <- 1:10 + i*10 - 10
          flds_i <- folds[fold_indices]
        }
        if (method == "nndm") {
          fold_index <- i*7 - 6
          idx_train <- folds[fold_index][[1]]
          idx_test <- folds[fold_index + 1][[1]]
          flds_i <- list("idx_train" = idx_train, "idx_test" = idx_test)
        }
        gd <- sampled_geodist(x = pts_sf, modeldomain = mask_, cvfolds = flds_i,
                              stat = 'density', samples = samples, showPlot = FALSE, cv_method = method)

        if(return_EMD) {
          gd_discrete <- emodis::discrete_curve(gd)
          s2s_s2p <- EMD(gd_discrete, "sample-to-sample", "sample-to-prediction")
          s2s_cv <- EMD(gd_discrete, "sample-to-sample", "CV-distances")
          s2p_cv <- EMD(gd_discrete, "sample-to-prediction", "CV-distances")

          df <- rbind(df, c(method, smpl, iteration, RMSE, RMSE_val, s2s_s2p, s2s_cv, s2p_cv))
          geodist_list[[x]] <- list("method"=method, "sample"=smpl, "geodist"=gd, "RMSE"=RMSE, "RMSE_val"=RMSE_val, "s2s_s2p"=s2s_s2p, "s2s_cv"=s2s_cv, "s2p_cv"=s2p_cv)
          x <- x+1
        } else {
          df <- rbind(df, c(method, smpl, iteration, RMSE, RMSE_val, NA, NA, NA))
          geodist_list[[x]] <- list("method"=method, "sample"=smpl, "geodist"=gd, "RMSE"=RMSE, "RMSE_val"=RMSE_val)
          x <- x+1
        }


      }
    }
  }

  names(df) <- c("method", "sample", "iteration", "RMSE", "RMSE_val", "s2s_s2p", "s2s_cv", "s2p_cv")
  df$RMSE <- as.numeric(df$RMSE)
  df$RMSE_val <- as.numeric(df$RMSE_val)
  df$s2s_s2p <- as.numeric(df$s2s_s2p)
  df$s2s_cv <- as.numeric(df$s2s_cv)
  df$s2p_cv <- as.numeric(df$s2p_cv)
  df$method <- factor(df$method, levels = c('random','spatial','nndm'),ordered = TRUE)
  df$sample <- factor(df$sample, levels = c('simpleRandom','regular','clusterMedium',
                                        'clusterStrong', 'clusterGapped'),ordered = TRUE)
  df$r_RMSE <- 100 * (df$RMSE - df$RMSE_val) / df$RMSE_val


  if(return_type == "df") {
    return(df)
  } else if (return_type == "geodist") {
    return(geodist_list)
  } else {
    print("wrong return type defined")
  }
}
