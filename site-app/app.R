############
# Preamble #
############

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(ggplot2)
library(dplyr)
library(DT)
library(arrow)
library(gridExtra)  # For arranging plots

# Load Arrow dataset
shiny_app_df <- open_dataset("shiny_app_df.parquet")

#######
# UI  #
#######
source("ui_funcs.R", local = TRUE)

ui <- fluidPage(
  tags$head(includeHTML("google-analytics.html")),
  navbarPage(
    title = "Site_Confidence",
    theme = shinytheme("flatly"),
    tabPanel("Plots", view_plot_page()),
    tabPanel("About this site", view_about_page())
  )
)

################
# Server logic #
################

server <- function(input, output, session) {

  # Dynamic cluster input
  output$input_clusters <- renderUI({
    req(input$spatial == "clustered")
    selectInput("num_clust", "Number of Clusters", c(1, 2, 3))
  })

  # Dynamically update step_size choices
  observeEvent(input$spatial, {
    if (input$spatial == "random") {
      updateSelectInput(session, "step_size",
                        choices = c("5 meter" = 5,
                                    "20 meters" = 20,
                                    "43 meters" = 43,
                                    "50 meters" = 50,
                                    "62.6 meters" = 62.6),
                        selected = 43)
    } else {
      updateSelectInput(session, "step_size",
                        choices = c("3 meter" = 3,
                                    "7 meter" = 7,
                                    "20 meter" = 20),
                        selected = 3)
    }
  })

  # ---- Reactive: filtered dataset ----
  filtered_df <- reactive({
    df_arrow <- shiny_app_df

    # Arrow-compatible filtering
    if (input$spatial == "random") {
      df_arrow <- df_arrow %>% filter(is.na(num_clust))
    } else {
      this_clust <- as.numeric(input$num_clust)
      df_arrow <- df_arrow %>% filter(num_clust == this_clust)
    }

    df_arrow <- df_arrow %>%
      filter(
        n_traps == as.numeric(input$num_traps),
        lure_attract == as.numeric(input$lure_attract),
        step_size == as.numeric(input$step_size)
      )

    # Pull into R
    df_r <- collect(df_arrow)

    # R-side transformations
    df_r %>%
      mutate(
        pests_per_ha = num_pests / 10,
        cat_ftw1 = ifelse(cat_ftw1 == "very low", "low", cat_ftw1),
        cat_ftw1 = factor(cat_ftw1, levels = c("high", "moderate", "low", "negligible"))
      )
  }) %>%
    bindCache(input$num_traps, input$lure_attract, input$step_size, input$spatial, input$num_clust)

  # ---- Plot ----
  output$plot <- renderPlot({
    p_df <- filtered_df()
    if (nrow(p_df) == 0) {
      plot.new()
      text(0.5, 0.5, "No data for selected filters", cex = 1.5)
      return()
    }

    # Line + ribbon plot
    p1 <- p_df %>%
      group_by(pests_per_ha) %>%
      summarise(
        ftw1_mn = mean(ftw1),
        ftw1_lb = quantile(ftw1, 0.05),
        ftw1_ub = quantile(ftw1, 0.95),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = pests_per_ha, y = ftw1_mn)) +
      geom_line(size = 2) +
      geom_ribbon(aes(ymin = ftw1_lb, ymax = ftw1_ub), alpha = 0.2) +
      xlab("True pest prevalence (pests per hectare)") +
      ylab("Trap catch (P/T/W)") +
      theme_bw()

    # Boxplot
    p2 <- p_df %>%
      ggplot(aes(y = cat_ftw1, x = pests_per_ha, fill = cat_ftw1)) +
      geom_boxplot() +
      xlab("True pest prevalence (pests per hectare)") +
      ylab("Trap catch (P/T/W)") +
      theme_bw()

    # Arrange plots side by side
    gridExtra::grid.arrange(p1, p2, nrow = 1)
  })

  # ---- Table ----
  output$values_table <- renderDT({
    table_df <- filtered_df() %>%
      group_by(cat_ftw1) %>%
      summarise(
        val = quantile(pests_per_ha, input$prob_perc / 100),
        .groups = "drop"
      ) %>%
      arrange(cat_ftw1) %>%
      mutate(val = paste0(round(val, 1), " pests/ha"))

    colnames(table_df) <- c("Category", paste0(input$prob_perc, "% Quantile"))

    datatable(table_df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
}

################
# Run the app  #
################

shinyApp(ui = ui, server = server)
