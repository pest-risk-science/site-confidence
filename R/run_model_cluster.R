
#' Runs the model clustered
#'
#' @description
#' This function runs a specific scenario
#'
#' @param num_seed description
#' @param num_trap description
#' @param lure_att description
#' @param step_size description
#' @param g_0 description
#'
#' @return Nothing
#' @export
run_model_cluster <- function(num_seed,
                      num_trap,
                      lure_att,
                      step_size,
                      num_cluster){

  run_time <- 28
  time_int_check <- 7

  this_sdm <- NULL
  this_dat <- simulate_clustered_poisson(total_points=num_seed,
                                         lambda_parent=num_cluster)
  this_dat$Fate <- rep(1,num_seed)
  this_dat$Age <- rep(1,num_seed)


  # put trap locations
  if(num_trap==1) {
    trap_locs <- data.frame(x = c(158.1139), y = c(158.1139))
  }
  if(num_trap==3) {
    trap_locs <- data.frame(x = c(105.4093, 210.8185, 158.1139),
                           y = c(210.8185, 210.8185, 105.4093))
  }
  if(num_trap==5) {
    trap_locs <-  data.frame(x = c(79.0595, 79.0595, 237.1709, 237.1709, 158.1139),
                            y = c(79.0595, 237.1709, 79.0595, 237.1709, 158.1139))
  }
  if(num_trap==10) {
    trap_locs <- data.frame(x = c(79.0595, 158.1139, 237.1709, 63.24556, 126.4911,
                                 189.7367, 252.9822, 79.0595, 158.1139, 237.1709),
                           y =  c(79.0595, 79.0595, 79.0595, 158.1139, 158.1139,
                                  158.1139, 158.1139, 237.1709, 237.1709, 237.1709))
  }

  this_sim <- sim_capture(init_dat = this_dat,
                          N_seed = num_seed,
                          #ntraps = num_trap,
                          surv_loc = trap_locs,
                          lam = 1/lure_att,
                          step_size_ad = step_size,
                          bbox = c(0,0,316.2278,316.2278),
                          Time = run_time,
                          random_length = TRUE,
                          allow_leave = FALSE,
                          attractive_areas = FALSE,
                          sdm = this_sdm)

  total_cap <- this_sim$total_captured
  if(length(total_cap) < run_time + 1) {
    last_cap <- rep(total_cap[length(total_cap)],
                    run_time + 1 - length(total_cap))
    total_cap <- c(total_cap,last_cap)
  }

  total_cap[seq(from=time_int_check,
                to = run_time,
                by = time_int_check) + 1]
}




#' Replicate the model run cluster version
#'
#' @description
#' Wrapper function to replicate `run_model_cluster()`
#'
#' @param num_replic description
#' @param HPC_run description
#' @param HPC_ind description
#' @param Scenario_ind description
#' @param Scenarios description
#'
#' @return Nothing
#' @export
replicate_model_run_cluster <- function(num_replic = num_replicates,
                                HPC_run = hpc_run,
                                HPC_ind = hpc_ind,
                                Scenario_ind = scenario_ind,
                                Scenarios = scenarios_clust) {

  if(HPC_run) {
    this_scenario_ind <- HPC_ind
  } else {
    this_scenario_ind <- Scenario_ind
  }

  this_trap <- Scenarios$n_traps[this_scenario_ind]
  this_step_size <- Scenarios$step_size[this_scenario_ind]
  this_lure <- Scenarios$lure_attract[this_scenario_ind]
  this_num_pests <- Scenarios$num_pests[this_scenario_ind]
  this_num_clust <- Scenarios$num_clust[this_scenario_ind]

  model_repped <- replicate(num_replic,
                            run_model_cluster(num_seed = this_num_pests,
                                              num_trap = this_trap,
                                              lure_att = this_lure,
                                              step_size = this_step_size,
                                              num_cluster = this_num_clust)
  )

  return_df <- t(model_repped)
  num_times <- ncol(return_df)

  return_df <- as.data.frame(return_df)
  names(return_df) <- paste0("time",1:num_times)

  return_df$num_pests <- this_num_pests
  return_df$n_traps <- this_trap
  return_df$step_size <- this_step_size
  return_df$lure_attract <- this_lure
  return_df$num_clust <- this_num_clust
  return_df$replication <- 1:num_replic
  return_df

}








