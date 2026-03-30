### Author: D. Gladish
### Note: This script incorporates the simulation results of main.R (run that
### first) and aggregates into tables, preparing to plot into figures through
### figures.R



############
# Preamble #
############

# Directories
root_dir <- getwd()
func_dir <- file.path(root_dir, "R")
data_dir <- file.path(root_dir, "data")
results_dir <- file.path(root_dir, "results")
results_dir_clust <- file.path(root_dir, "results_clust")
tables_dir <- file.path(root_dir, "tables")
figures_dir <- file.path(root_dir, "figures")

# Load Functions
invisible(sapply(list.files(func_dir, "\\.[Rr]$", full.names = TRUE),
                 source, encoding = "UTF-8"))

# Install and load packages
load_my_packages()

# Load Scenarios
scenarios <- read.csv(file.path(data_dir, "scenarios.csv"))
scenarios_clust <- read.csv(file.path(data_dir, "scenarios_clust.csv"))

# Create Data frame
create_df <- FALSE
if(create_df) {
  res_df <- get_results_df()
  res_df <- get_ftw(res_df)
  res_df <- bin_ftw(res_df)

  # Adding zeros
  zero_df <- zero_run(res_df,scenarios)
  res_df <- rbind(res_df,zero_df)

  write.csv(res_df,"data/res_df.csv")

} else {
  res_df <- read.csv("data/res_df.csv")
}

create_df_clust <- FALSE
if(create_df_clust) {
  res_df_clust <- get_results_df(res_dir = results_dir_clust)
  res_df_clust <- get_ftw(res_df_clust)
  res_df_clust <- bin_ftw(res_df_clust)

  # Adding zeros
  zero_df_clust <- zero_run(res_df_clust,scenarios_clust)
  res_df_clust <- rbind(res_df_clust,zero_df_clust)

  write.csv(res_df_clust,"data/res_df_clust.csv")

} else {
  res_df_clust <- read.csv("data/res_df_clust.csv")
}



#################
# Random Forest #
#################

# Recommend running this on RStudio Server or Cluster
rf_run <- FALSE
if(rf_run){
  library(ranger)
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  root_dir <- "/datasets/work/d61-drought-ind/work/Dan/other_projects/Biosecurity_site_confidence"
  data_dir <- file.path(root_dir,"data")
  setwd(root_dir)

  # Reading in Data
  res_df <- read.csv(file.path(data_dir,"res_df.csv"))
  res_df_clust <- read.csv(file.path(data_dir,"res_df_clust.csv"))

  # Random Forest
  formi1 <- "ftw1 ~ num_pests + n_traps + step_size + lure_attract + same_spot"
  formi2 <- "num_pests ~ ftw1 + n_traps + step_size + lure_attract + same_spot"

  formi1_clust <- "ftw1 ~ num_pests + n_traps + step_size + lure_attract + num_clust"
  formi2_clust <- "num_pests ~ ftw1 + n_traps + step_size + lure_attract + num_clust"


  rf_1 <- ranger(formi1, data=res_df, importance="permutation",
                 num.trees = 100)
  rf_2 <- ranger(formi2, data=res_df, importance="permutation",
                 num.trees = 100)

  rf_1_clust <- ranger(formi1_clust, data=res_df_clust, importance="permutation",
                       num.trees = 100)
  rf_2_clust <- ranger(formi2_clust, data=res_df_clust, importance="permutation",
                       num.trees = 100)


  # Data Wrangling
  var_imp_df <- data.frame(variable = names(rf_1$variable.importance),
                           nonclust_ftw = rf_1$variable.importance,
                           nonclust_count = rf_2$variable.importance,
                           clust_ftw = rf_1_clust$variable.importance,
                           clust_count = rf_2_clust$variable.importance)
  var_imp_df <- var_imp_df[-1,]
  var_imp_df$variable <- c("Number of traps", "Step Size", "Lure attractiveness",
                           "Pest initialsation")
  names(var_imp_df) <- c("Variable",
                         "Nonclustered case, PTW",
                         "Nonclustered case, Pest prevalance",
                         "Clustered case, PTW",
                         "Clustered case, Pest prevalance")
  var_imp_df <- var_imp_df %>%
    pivot_longer(cols=c("Nonclustered case, PTW",
                        "Nonclustered case, Pest prevalance",
                        "Clustered case, PTW",
                        "Clustered case, Pest prevalance"),
                 names_to = "Model")
  var_imp_df$Model <- factor(var_imp_df$Model,
                             levels = c("Nonclustered case, PTW",
                                        "Clustered case, PTW",
                                        "Nonclustered case, Pest prevalance",
                                        "Clustered case, Pest prevalance"))

  saveRDS(var_imp_df, file = file.path(data_dir,"var_imp_df.rds"))
} else {
  var_imp_df <- readRDS(file.path(data_dir,"var_imp_df.rds"))
}



##############
# Validation #
##############

# Only run this if needed to be redone
valid_run <- FALSE
if(valid_run){
  # Creating the initial values of the simulation
  create_grid <- function(d, grid_res = 8, num_pests = 2000) {

    # Survey Locations
    x_loc <- seq(from = d, to = d*grid_res, by = d)
    y_loc <- seq(from = d, to = d*grid_res, by = d)
    surv_loc <- expand.grid(x = x_loc, y = y_loc)

    # Bbox
    bbox <- c(0, 0, d*(grid_res+1), d*(grid_res+1))

    # Starting outbreak data
    init_loc <- c(d*(grid_res + 1)/2, d*(grid_res + 1)/2)
    init_dat <-  data.frame(x = rep(init_loc[1], num_pests),
                            y = rep(init_loc[2], num_pests),
                            Age = rep(1, num_pests))

    list(surv_loc = surv_loc,
         bbox = bbox,
         init_dat = init_dat)
  }

  grid_150 <- create_grid(150)
  grid_100 <- create_grid(100)
  grid_75 <- create_grid(75)

  # Creating meta df
  meta_df1 <- data.frame(species = c("OFF", "OFF", "OFF", "OFF", "OFF", "OFF",
                                     "MED", "MED", "MED", "MED", "MED", "MED",
                                     "OFF", "OFF", "MED", "MED"),
                         grid_d = c(150, 150, 150, 75, 75, 75, 150, 150, 150,
                                    75, 75, 75, 100, 100, 100, 100),
                         p = c(0.519, 0.649, 0.702, 0.904, 0.841, 0.956, 0.104,
                               0.112, 0.177, 0.374, 0.301, 0.243, 0.746, 0.871,
                               0.084, 0.036),
                         lam = c(34.94, 41.49, 44.58, 31.59, 28.15, 37.75, 14.46,
                                 15.08, 18.90, 14.20, 12.45, 10.97, 31.70, 38.87,
                                 8.25, 5.65))
  meta_df2 <- data.frame(species = rep("melon", 12),
                         grid_d = c(150, 150, 150, 75, 75, 75, 150, 150,
                                    150,  75, 75, 75),
                         p = c(0.203, 0.275, 0.343, 0.452, 0.383, 0.348,
                               0.138, 0.140, 0.213, 0.374, 0.353, 0.294),
                         lam = c(20.26, 23.83, 26.94, 16.01, 14.41, 13.58,
                                 16.83, 16.94, 20.79, 14.20, 13.70, 12.28))
  meta_df <- rbind(meta_df1, meta_df2)
  step_size_map <- data.frame(species = c("OFF", "MED", "melon"),
                              step_size = c(62.6, 42.9, 50))
  meta_df <- merge(meta_df, step_size_map)

  # capture function
  run_sim_cap <- function(dat, grid75 = grid_75, grid100 = grid_100,
                           grid150 = grid_150, g_0 = 1,
                           day_cap = 2) {

    if(dat$grid_d == 75){this_grid <- grid75}
    if(dat$grid_d == 100){this_grid <- grid100}
    if(dat$grid_d == 150){this_grid <- grid150}

    new_dat <- this_grid$init_dat
    new_dat$Fate <- 1
    this_spread <- sim_spread(init_dat = new_dat,
                              rand.walk = TRUE,
                              step_size_ad = dat$step_size,
                              Time = day_cap,
                              offspr_mu = 0,
                              bbox = this_grid$bbox,
                              random_length = FALSE,
                              PLOT.IT = FALSE)

    this_sim <- sim_capture(init_dat = this_spread$dat[[day_cap + 1]],
                            surv_loc = this_grid$surv_loc,
                            bbox = this_grid$bbox,
                            Time = 1,
                            g0 = g_0,
                            step_size_ad = 0,
                            lam = 1/dat$lam,
                            random_length = TRUE)

    return_val <- this_sim$total_captured[2]/2000
    return_val
  }

  num_rep <- 50

  sim_prop_res <- matrix(NA, nrow(meta_df), num_rep)

  for(ind in 1:nrow(meta_df)) {
    cat("***iteration", ind, "\n")
    sim_prop_res[ind,] <- replicate(num_rep, run_sim_cap(meta_df[ind,]))
  }


  meta_df$p_hat <- apply(sim_prop_res,1,mean)
  meta_df$p_lb <- apply(sim_prop_res,1,quantile,.025)
  meta_df$p_ub <- apply(sim_prop_res,1,quantile,.975)


  saveRDS(sim_prop_res,file = file.path(data_dir, "sim_prop_res.rds"))
  saveRDS(meta_df, file = file.path(data_dir, "meta_df.rds"))

} else {
  sim_prop_res <- readRDS(file.path(data_dir, "sim_prop_res.rds"))
  meta_df <- readRDS(file.path(data_dir, "meta_df.rds"))
}



##################
# Summary Tables #
##################

# With Step size
sum_df <- res_df %>%
  filter(attract_area==FALSE) %>%
  mutate(cat_ftw1 = replace(cat_ftw1, cat_ftw1=="very low","low")) %>%
  summarise(pests_mn = mean(num_pests),
            pests_95 = quantile(num_pests,.95),
            pests_99 = quantile(num_pests,.99),
            .by = c(n_traps,step_size,lure_attract,cat_ftw1))


sum_df_clust <- res_df_clust %>%
  mutate(cat_ftw1 = replace(cat_ftw1, cat_ftw1=="very low","low")) %>%
  summarise(pests_mn = mean(num_pests),
            pests_95 = quantile(num_pests,.95),
            pests_99 = quantile(num_pests,.99),
            .by = c(n_traps,step_size,lure_attract,num_clust,cat_ftw1))


sum_df_mn <- sum_df %>%
  dplyr::select(n_traps,step_size,lure_attract,cat_ftw1,pests_mn) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_mn)

sum_df_95 <- sum_df %>%
  dplyr::select(n_traps,step_size,lure_attract,cat_ftw1,pests_95) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_95)

sum_df_99 <- sum_df %>%
  dplyr::select(n_traps,step_size,lure_attract,cat_ftw1,pests_99) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_99)

sum_df_clust_mn <- sum_df_clust %>%
  dplyr::select(n_traps,step_size,lure_attract,num_clust,cat_ftw1,pests_mn) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_mn)

sum_df_clust_95 <- sum_df_clust %>%
  dplyr::select(n_traps,step_size,lure_attract,num_clust,cat_ftw1,pests_95) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_95)

sum_df_clust_99 <- sum_df_clust %>%
  dplyr::select(n_traps,step_size,lure_attract,num_clust,cat_ftw1,pests_99) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_99)


# Without step size
sum_df_v2 <- res_df %>%
  filter(attract_area==FALSE) %>%
  mutate(cat_ftw1 = replace(cat_ftw1, cat_ftw1=="very low","low")) %>%
  summarise(pests_mn = mean(num_pests),
            pests_95 = quantile(num_pests,.95),
            pests_99 = quantile(num_pests,.99),
            .by = c(n_traps,lure_attract,cat_ftw1))


sum_df_clust_v2 <- res_df_clust %>%
  mutate(cat_ftw1 = replace(cat_ftw1, cat_ftw1=="very low","low")) %>%
  summarise(pests_mn = mean(num_pests),
            pests_95 = quantile(num_pests,.95),
            pests_99 = quantile(num_pests,.99),
            .by = c(n_traps,lure_attract,num_clust,cat_ftw1))


sum_df_mn_v2 <- sum_df_v2 %>%
  dplyr::select(n_traps,lure_attract,cat_ftw1,pests_mn) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_mn)

sum_df_95_v2  <- sum_df_v2 %>%
  dplyr::select(n_traps,lure_attract,cat_ftw1,pests_95) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_95)

sum_df_99_v2  <- sum_df_v2 %>%
  dplyr::select(n_traps,lure_attract,cat_ftw1,pests_99) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_99)

sum_df_clust_mn_v2  <- sum_df_clust_v2 %>%
  dplyr::select(n_traps,lure_attract,num_clust,cat_ftw1,pests_mn) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_mn)

sum_df_clust_95_v2  <- sum_df_clust_v2 %>%
  dplyr::select(n_traps,lure_attract,num_clust,cat_ftw1,pests_95) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_95)

sum_df_clust_99_v2  <- sum_df_clust_v2 %>%
  dplyr::select(n_traps,lure_attract,num_clust,cat_ftw1,pests_99) %>%
  tidyr::pivot_wider(names_from = cat_ftw1, values_from = pests_99)

# Writing tables
write.csv(sum_df_mn,file.path(tables_dir,"sum_df_mn.csv"))
write.csv(sum_df_95,file.path(tables_dir,"sum_df_95.csv"))
write.csv(sum_df_99,file.path(tables_dir,"sum_df_99.csv"))
write.csv(sum_df_clust_mn,file.path(tables_dir,"sum_df_clust_mn.csv"))
write.csv(sum_df_clust_95,file.path(tables_dir,"sum_df_clust_95.csv"))
write.csv(sum_df_clust_99,file.path(tables_dir,"sum_df_clust_99.csv"))

write.csv(sum_df_mn_v2,file.path(tables_dir,"sum_df_mn_v2.csv"))
write.csv(sum_df_95_v2,file.path(tables_dir,"sum_df_95_v2.csv"))
write.csv(sum_df_99_v2,file.path(tables_dir,"sum_df_99_v2.csv"))
write.csv(sum_df_clust_mn_v2,file.path(tables_dir,"sum_df_clust_mn_v2.csv"))
write.csv(sum_df_clust_95_v2,file.path(tables_dir,"sum_df_clust_95_v2.csv"))
write.csv(sum_df_clust_99_v2,file.path(tables_dir,"sum_df_clust_99_v2.csv"))
