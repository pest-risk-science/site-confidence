view_plot_page <- function() {
  sidebarLayout(
    sidebarPanel(
      selectInput("num_traps",
                  "Number of traps",
                  c("1 Trap" = 1,
                    "3 Traps" = 3,
                    "5 Traps" = 5,
                    "10 Traps" = 10),
                  selected = 1),
      selectInput("lure_attract",
                  "Lure attractiveness",
                  c("7 meters" = 7,
                    "14 meters" = 14,
                    "25 meters" = 25,
                    "36 meters" = 36,
                    "50 meters" = 50),
                  selected = 14),
      selectInput("step_size",
                  "Mean step Size",
                  c("5 meter" = 5,
                    "20 meters" = 20,
                    "43 meters" = 43,
                    "50 meters" = 50,
                    "62.6 meters" = 62.6),
                  selected = 43),
      selectInput("spatial",
                  "Random or Clustered?",
                  c("Random" = "random",
                    "Clustered" = "clustered"),
                  selected = "random"),
      conditionalPanel(
        condition = "input.spatial == 'clustered'",
        uiOutput("input_clusters")
      ),
      numericInput("prob_perc",
                   "Percent confidence of pests below",
                   value = 95,
                   min = 1, max = 100,
                   step = 0.1)
    ),
    mainPanel(
      plotOutput("plot"),
      DTOutput("values_table")
    )
  )
}

view_about_page <- function() {
  tags$div(
    tags$h4("About"),
    "TO FILL IN"
  )
}
