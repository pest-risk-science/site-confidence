############
# Preamble #
############

# Packages
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyBS)
library(htmltools)
library(ggplot2)
library(dplyr)

# Data
res_df <- read.csv("res_df.csv")

# Functions
###source("", local = TRUE)

######
# UI #
######
source("ui_funcs.R", local = TRUE)

ui <- fluidPage(
  navbarPage(title = "Site Confidence", theme = shinytheme("flatly"),
             tabPanel("Plot", view_plot_page()),
             tabPanel("About this site", view_about_page())
  )
)


##########
# Server #
##########

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$p1 <- renderPlot({
      p1_df <- res_df %>%
        filter(n_traps == input$n_trap_p1,
               step_size == input$step_size_p1,
               lure_attract == input$lure_p1,
               g0 == input$g0_p1) %>%
        select(num_pests, any_of(input$xaxis_p1))
      names(p1_df) <- c("num_pests","ftw")
      ggplot(p1_df, aes(x=ftw, y = num_pests)) +
        stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
        scale_fill_continuous(type = "viridis") +
        geom_abline() +
        xlab("Flies/trap/week") +
        ylab("True Number of Pests")
    })
    output$p2 <- renderPlot({
      p2_df <- res_df %>%
        filter(n_traps == input$n_trap_p2,
               step_size == input$step_size_p2,
               lure_attract == input$lure_p2,
               g0 == input$g0_p2) %>%
        select(num_pests, any_of(input$xaxis_p2))
      names(p2_df) <- c("num_pests","ftw")
      ggplot(p2_df, aes(x=ftw, y = num_pests)) +
        stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
        scale_fill_continuous(type = "viridis") +
        geom_abline() +
        xlab("Flies/trap/week") +
        ylab("True Number of Pests")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
