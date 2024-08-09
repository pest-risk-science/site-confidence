
# Plot page
view_plot_page <- function() {
  page = fluidPage(
    fluidRow(
      column(width = 6,
             h4("Plot 1"),
             withSpinner(plotOutput("p1"))),
      column(width = 6,
             h4("Plot 2"),
             withSpinner(plotOutput("p2")))
    ),
    fluidRow(
      fluidRow(
        column(width = 6,
               selectInput("xaxis_p1",
                           "Flies/Trap/Week:",
                           c("Week 1" = "ftw1",
                             "Week 2" = "ftw2",
                             "Week 3" = "ftw3",
                             "Week 4" = "ftw4",
                             "Mean FTW" = "mean_ftw")),
               selectInput("n_trap_p1",
                           "Number of Traps",
                           c("1 Trap" = 1,
                             "5 Traps" = 5,
                             "10 Traps" = 10)),
               selectInput("g0_p1",
                           "P(trappability)",
                           c("0.1" = 0.1,
                             "0.25" = 0.25,
                             "0.5" = 0.5,
                             "0.75" = 0.75,
                             "1" = 1)),
               selectInput("lure_p1",
                           "Lure Attractiveness:",
                           c("5" = 5,
                             "25" = 25,
                             "36" = 36)),
               selectInput("step_size_p1",
                           "Mean Step Size of Pest",
                           c("5" = 5,
                             "20" = 20,
                             "43" = 43))
        ),
        column(width = 6,
               selectInput("xaxis_p2",
                           "Flies/Trap/Week:",
                           c("Week 1" = "ftw1",
                             "Week 2" = "ftw2",
                             "Week 3" = "ftw3",
                             "Week 4" = "ftw4",
                             "Mean FTW" = "mean_ftw")),
               selectInput("n_trap_p2",
                           "Number of Traps",
                           c("1 Trap" = 1,
                             "5 Traps" = 5,
                             "10 Traps" = 10)),
               selectInput("g0_p2",
                           "P(trappability)",
                           c("0.1" = 0.1,
                             "0.25" = 0.25,
                             "0.5" = 0.5,
                             "0.75" = 0.75,
                             "1" = 1)),
               selectInput("lure_p2",
                           "Lure Attractiveness:",
                           c("5" = 5,
                             "25" = 25,
                             "36" = 36)),
               selectInput("step_size_p2",
                           "Mean Step Size of Pest",
                           c("5" = 5,
                             "20" = 20,
                             "43" = 43))
        )
      )
    )
  )
}

# About the site page
view_about_page <- function() {
  tags$div(
    tags$h4("About"),
    "TO FILL IN"
  )
}
