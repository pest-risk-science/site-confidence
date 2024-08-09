#' Create results data frame
#'
#' @description
#' Collect all csv files of model runs and puts them into one data frame
#'
#' @param res_dir description
#'
#' @return Nothing
#' @export
get_results_df <- function(res_dir = results_dir) {
  res_files <- list.files(res_dir, pattern=".csv",full.names = TRUE)
  results_df <- do.call(rbind,lapply(res_files,read.csv))

  keep_nms <- names(results_df)
  keep_nms <- keep_nms[!(keep_nms%in%"X")]

  results_df[,keep_nms]
}

#' Get flies per trap per week
#'
#' @description
#' Creates new flies per trap per week variables in a results data frame
#'
#' @param results_df description
#'
#' @return Nothing
#' @export
get_ftw <- function(results_df) {

  df_nms <- names(results_df)
  time_ind <- grep("time",df_nms)
  n_time <- length(time_ind)

  results_df$ftw1 <- results_df$time1/results_df$n_traps

  for(ind in 2:n_time) {
    flies_caught <- results_df[[paste0("time",ind)]] - results_df[[paste0("time",ind - 1)]]
    results_df[[paste0("ftw",ind)]] <- flies_caught / results_df$n_traps
  }

  results_df
}


