hpc_ind <- as.numeric(commandArgs(trailingOnly = TRUE))

############
# Preamble #
############

# Directories
root_dir <- getwd()
func_dir <- file.path(root_dir, "R")
data_dir <- file.path(root_dir, "data")
result_dirs <- file.path(root_dir, "results")

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

# Num pests
num_pests <- c(1, 5, 10, 20, 25, seq(50,1000,by=25))

# Scenarios
if(generate_new_scenarios) {
  scenarios <- generate_scenarios()
}
if(write_new_scenarios) {
  write.csv2(scenarios,file.path(data_dir,"scenarios.csv"))
} else {
  scenarios <- read.csv2(file.path(data_dir,"scenarios.csv"))
}

# Run the model
model_run_df <- replicate_model_run()

# Write results
file_ind <- scenario_ind
if(hpc_run) {file_ind <- hpc_ind}
file_name <- paste0("traps_", scenarios$n_traps[file_ind],
                    "_step_", scenarios$step_size[file_ind],
                    "_lure_", scenarios$lure_attract[file_ind],
                    ".csv")
write.csv2(model_run_df,file.path(results_dir,file_name))



