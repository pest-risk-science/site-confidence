
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




#' Bin flies per trap per week
#'
#' @description
#' Puts flies per trap per week into categories
#'
#' @param results_df description
#'
#' @return Nothing
#' @export
bin_ftw <- function(results_df) {

  df_nms <- names(results_df)
  time_ind <- grep("ftw",df_nms)
  n_time <- length(time_ind)

  my_breaks <- c(-Inf, 0.01, 0.1, 1, 10, Inf)
  my_labels <- c("negligible", "very low", "low",
                 "moderate", "high")

  results_df$cat_ftw1 <-
    cut(results_df$ftw1,
        breaks = my_breaks,
        labels = my_labels
    )

  for(ind in 2:n_time) {
    results_df[[paste0("cat_ftw",ind)]] <-
      cut(results_df[[paste0("ftw",ind)]],
          breaks = my_breaks,
          labels = my_labels)
  }
  results_df
}





#' Creating scenarios with zero
#'
#' @description
#' Add in the zeros!
#'
#' @param results_df description
#' @param scenario_df descriptino
#'
#' @return Nothing
#' @export
zero_run <- function(results_df, scenarios_df) {
  require(dplyr)
  num_rep <- max(results_df$replication)
  zero_df <- data.frame(time1 = rep(0,num_rep),
                        time2 = rep(0,num_rep),
                        time3 = rep(0,num_rep),
                        time4 = rep(0,num_rep),
                        ftw1 = rep(0,num_rep),
                        ftw2 = rep(0,num_rep),
                        ftw3 = rep(0,num_rep),
                        ftw4 = rep(0,num_rep),
                        cat_ftw1 = rep("negligible",num_rep),
                        cat_ftw2 = rep("negligible",num_rep),
                        cat_ftw3 = rep("negligible",num_rep),
                        cat_ftw4 = rep("negligible",num_rep),
                        replication = 1:num_rep)

  scenarios_df_sub <- scenarios_df %>%
    select(!c(num_pests,X))
  scenarios_df_sub <- unique(scenarios_df_sub)
  scenarios_df_sub$num_pests <- 0

  zero_df <- merge(zero_df, scenarios_df_sub)
  zero_df

}
