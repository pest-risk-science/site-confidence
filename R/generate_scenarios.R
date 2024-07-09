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
  num_traps <- c(1,5,10)
  step_sizes <- c(5,20,43)
  lure_attract <- c(5,25,36)
  return(
    expand.grid(n_traps = num_traps,
                step_size = step_sizes,
                lure_attract = lure_attract)
    )
}

#' Function to read in


