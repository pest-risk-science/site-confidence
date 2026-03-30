# -	Go through low, medium, and high trapping scenarios
# -	Low, medium, and high dispersal rates
# -	Low, medium, and high lure attractiveness rates
# - range 1 - 1000


#' Generate scenarios
#'
#' @description
#' Function to get combinations of scenarios
#'
#' @return
#' Data frame of number of traps, step sizes, lure attractiveness, and number
#' of pests in the outbreak.
#' @export
generate_scenarios <- function(){
  num_traps <- c(1,3,5,10)
  step_sizes <- c(5,20,43,50,62.6)
  lure_attract <- c(7,14,25,36,50)
  #same_spots <- c(TRUE,FALSE)
  #attract_areas <- c(TRUE,FALSE)
  #g0 <- c(.01,.1,.25,.5,.75,1)

  num_pests <- seq(5,1000,by=5)
  return(
    expand.grid(n_traps = num_traps,
                step_size = step_sizes,
                lure_attract = lure_attract,
                #same_spot = same_spots,
                #attract_area = attract_areas,
                #g0 = g0,
                num_pests = num_pests)
    )
}



#' Generate scenarios for clustering
#'
#' @description
#' Function to get combinations of scenarios
#'
#' @return
#' Data frame of number of traps, step sizes, lure attractiveness, and number
#' of pests in the outbreak.
#' @export

generate_scenarios_cluster <- function(){
  num_traps <- c(1,3,5,10)
  step_sizes <- c(3,7,20)
  lure_attract <- c(7,14,25,36,50)
  num_clust <- c(1,2,3,5)

  num_pests <- seq(5,1000,by=5)
  return(
    expand.grid(n_traps = num_traps,
                step_size = step_sizes,
                lure_attract = lure_attract,
                num_clust = num_clust,
                num_pests = num_pests)
  )
}
