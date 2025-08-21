hpc_ind <- as.numeric(commandArgs(trailingOnly = TRUE))

############
# Preamble #
############

# Directories
root_dir <- getwd()
func_dir <- file.path(root_dir, "R")
data_dir <- file.path(root_dir, "data")
results_dir <- file.path(root_dir, "results")
results_clust_dir <- file.path(root_dir, "results_clust")
plot_dir <- file.path(root_dir, "plots")

# Load Functions
invisible(sapply(list.files(func_dir, "\\.[Rr]$", full.names = TRUE),
                 source, encoding = "UTF-8"))

# Install and load packages
load_my_packages()

############
# Analysis #
############

# Flags
generate_new_scenarios <- FALSE
write_new_scenarios <- FALSE
hpc_run <- TRUE
num_replicates <- 500
scenario_ind <- 1
clustered_analysis <- TRUE


if(!clustered_analysis) {
  # Scenarios
  if(generate_new_scenarios) {
    scenarios <- generate_scenarios()
  }
  if(write_new_scenarios) {
    write.csv(scenarios,file.path(data_dir, "scenarios.csv"))
  } else {
    scenarios <- read.csv(file.path(data_dir, "scenarios.csv"))
  }

  # Run the model
  model_run_df <- replicate_model_run()

  # Write results
  file_ind <- scenario_ind
  if(hpc_run) {file_ind <- hpc_ind}
  file_name <- paste0("traps_", scenarios$n_traps[file_ind],
                      "_step_", scenarios$step_size[file_ind],
                      "_lure_", scenarios$lure_attract[file_ind],
                      "_spot_", scenarios$same_spot[file_ind],
                      "_attract_", scenarios$attract_area[file_ind],
                      "_npests_", scenarios$num_pests[file_ind],
                      ".csv")
  write.csv(model_run_df, file.path(results_dir,file_name))
}


if(clustered_analysis) {
  if(generate_new_scenarios){
    scenarios_clust <- generate_scenarios_cluster()
  }
  if(write_new_scenarios) {
    write.csv(scenarios_clust, file.path(data_dir, "scenarios_clust.csv"))
  } else {
    scenarios_clust <- read.csv(file.path(data_dir, "scenarios_clust.csv"))
  }

  # Run the model
  model_run_df_clust <- replicate_model_run_cluster()

  # write results
  # Write results
  file_ind <- scenario_ind
  if(hpc_run) {file_ind <- hpc_ind}
  file_name <- paste0("traps_", scenarios_clust$n_traps[file_ind],
                      "_step_", scenarios_clust$step_size[file_ind],
                      "_lure_", scenarios_clust$lure_attract[file_ind],
                      "_clust", scenarios_clust$num_clust[file_ind],
                      "_npests_", scenarios_clust$num_pests[file_ind],
                      ".csv")
  write.csv(model_run_df_clust, file.path(results_clust_dir,file_name))
}




