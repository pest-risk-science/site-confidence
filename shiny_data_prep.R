
############
# Preamble #
############
library(arrow)

root_dir <- getwd()
data_dir <- file.path(root_dir, "data")
shiny_dir <- file.path(root_dir, "site-app")

res_df <- read.csv(file.path(data_dir,"res_df.csv"))
res_df_clust <- read.csv(file.path(data_dir,"res_df_clust.csv"))

###############
# Creating DF #
###############

res_df_sub <- res_df %>%
  dplyr::filter(same_spot == FALSE,
         attract_area == FALSE) %>%
  dplyr::mutate(pests_per_ha = num_pests/10) %>%
  dplyr::select(pests_per_ha, n_traps, step_size, lure_attract, ftw1, cat_ftw1) %>%
  dplyr::mutate(num_clust = NA)


res_df_clust_sub <- res_df_clust %>%
  dplyr::filter(num_clust %in% c(1,2,3)) %>%
  dplyr::mutate(pests_per_ha = num_pests/10) %>%
  dplyr::select(pests_per_ha, n_traps, step_size, lure_attract, num_clust, ftw1, cat_ftw1)


shiny_df <- bind_rows(res_df_sub,
                      res_df_clust_sub)


##########
# Saving #
##########
write.csv(shiny_df, file.path(shiny_dir,"shiny_df.csv"), row.names=FALSE)


shiny_df %>%
  write_dataset(file.path(shiny_dir,"shiny_df.parquet"), format = "parquet",
                partitioning = c("num_clust"))
