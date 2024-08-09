
#' Runs the model
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
run_model <- function(num_seed,
                      num_trap,
                      lure_att,
                      step_size,
                      g_0){

  run_time <- 28
  time_int_check <- 7

  this_sim <- sim_capture(N_seed = num_seed,
                          ntraps = num_trap,
                          lam = 1/lure_att,
                          step_size_ad = step_size,
                          bbox = c(0,0,316.2278,316.2278),
                          Time = run_time,
                          g0 = g_0)

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




#' Replicate the model run
#'
#' @description
#' Wrapper function to replicate `run_model()`
#'
#' @param num_replic description
#' @param HPC_run description
#' @param HPC_ind description
#' @param Scenario_ind description
#' @param Scenarios description
#'
#' @return Nothing
#' @export
replicate_model_run <- function(num_replic = num_replicates,
                                HPC_run = hpc_run,
                                HPC_ind = hpc_ind,
                                Scenario_ind = scenario_ind,
                                Scenarios = scenarios) {

  if(HPC_run) {
    this_scenario_ind <- HPC_ind
  } else {
    this_scenario_ind <- Scenario_ind
  }

  this_trap <- Scenarios$n_traps[this_scenario_ind]
  this_step_size <- Scenarios$step_size[this_scenario_ind]
  this_lure <- Scenarios$lure_attract[this_scenario_ind]
  this_g0 <- Scenarios$g0[this_scenario_ind]
  this_num_pests <- Scenarios$num_pests[this_scenario_ind]

  model_repped <- replicate(num_replic,
                            run_model(num_seed = this_num_pests,
                                      num_trap = this_trap,
                                      lure_att = this_lure,
                                      step_size = this_step_size,
                                      g_0 = this_g0)
  )

  return_df <- t(model_repped)
  num_times <- ncol(return_df)

  return_df <- as.data.frame(return_df)
  names(return_df) <- paste0("time",1:num_times)

  return_df$num_pests <- this_num_pests
  return_df$n_traps <- this_trap
  return_df$step_size <- this_step_size
  return_df$lure_attract <- this_lure
  return_df$g0 <- this_g0
  return_df$replication <- 1:num_replic
  return_df

}








