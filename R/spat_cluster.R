
#' Simulates a Clustered Poisson Point Process
#'
#' @description
#' This function gets a simulated clustered Poisson point process with set
#' number of individuals
#'
#' @param total_points description
#' @param lambda_parent description
#' @param cluster_radius description
#' @param actual_box description
#' @param window description
#' @param max_retries description
#'
#' @return Data frame of locations
#' @export

simulate_clustered_poisson <- function(total_points, lambda_parent,
                                       cluster_radius = 0.015811,
                                       actual_box = c(0,0,316.2278,316.2278),
                                       window = owin(c(0, 1), c(0, 1)),
                                       max_retries = 10) {
  require(spatstat.random)
  attempt <- 1
  while (attempt <= max_retries) {
    tryCatch(
      {
        # Initialize storage for all points
        all_offspring <- data.frame(x = numeric(0), y = numeric(0))

        # Repeat until we reach the exact number of points
        while (nrow(all_offspring) < total_points) {
          remaining_points <- total_points - nrow(all_offspring)

          # Step 1: Generate parent points
          parent_points <- rpoispp(lambda = lambda_parent, win = window)
          num_parents <- parent_points$n

          if (num_parents == 0) next  # Skip if no parent points generated

          # Step 2: Allocate offspring to parents
          offspring_counts <- rmultinom(1, size = remaining_points, prob = rep(1, num_parents))

          # Step 3: Generate offspring around parents
          for (i in 1:num_parents) {
            if (offspring_counts[i] > 0) {
              # Generate offspring positions around the parent
              parent_x <- parent_points$x[i]
              parent_y <- parent_points$y[i]

              angles <- runif(offspring_counts[i], 0, 2 * pi)
              radii <- runif(offspring_counts[i], 0, cluster_radius)

              x <- parent_x + radii * cos(angles)
              y <- parent_y + radii * sin(angles)

              # Ensure offspring stay within the window
              valid <- (x >= window$xrange[1]) & (x <= window$xrange[2]) &
                (y >= window$yrange[1]) & (y <= window$yrange[2])
              x <- x[valid]
              y <- y[valid]

              all_offspring <- rbind(all_offspring, data.frame(x = x, y = y))
            }
          }
        }

        # Trim to the exact number of points
        all_offspring <- all_offspring[1:total_points, ]
        all_offspring[,1] <- all_offspring[,1] * actual_box[3]
        all_offspring[,2] <- all_offspring[,2] * actual_box[4]

        # Return the result as a ppp object
        return(all_offspring)
      },
      error = function(e) {
        # Print a warning and retry
        message(paste("Attempt", attempt, "failed. Retrying..."))
        attempt <<- attempt + 1
      }
    )
  }

  # If we exhaust retries, stop with an error
  stop("Failed to simulate the process after ", max_retries, " attempts.")
}








# Parameters
total_points <- 500    # Total number of points
lambda_parent <- 3          # Intensity of parent points
cluster_radius <- 0.05        # Radius of clusters

# Simulate the process
# For reproducibility
simulated_process <- simulate_clustered_poisson(total_points, lambda_parent, cluster_radius)

# Plot the result
plot(simulated_process, main = "Clustered Poisson Point Process")



length(simulated_process$x)

tmp_k <- kmeans(data.frame(x=simulated_process$x,y=simulated_process$y),centers = lambda_parent)
table(tmp_k$cluster)
