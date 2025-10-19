############
# Preamble #
############

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(DT)
library(arrow)
library(gridExtra)
#library(khroma)
#library(ggtext)

# Load Arrow dataset
shiny_app_df <- open_dataset("shiny_app_df.parquet")

#######
# UI  #
#######
source("ui_funcs.R", local = TRUE)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(includeHTML("google-analytics.html")),
  navbarPage(
    title = "Site_Confidence",
    theme = shinytheme("flatly"),
    tabPanel("Plots", view_plot_page()),
    tabPanel("Compare Scenarios", view_comparison_page()),
    tabPanel("About this site", view_about_page()),
    tabPanel("Contact", view_ref_page())
  )
)

################
# Server logic #
################

server <- function(input, output, session) {

  #############
  # PLOT PAGE #
  #############

  observe({
    if (!is.null(input$num_clust)) {
      shinyBS::addTooltip(session, "num_clust", cluster_tt, placement = "top")
    }
  })

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
    shinyBS::addTooltip(session, "step_size", step_size_tt, placement = "top")
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
        cat_ftw1 = factor(cat_ftw1, levels = c("negligible", "low", "moderate", "high"))
      ) %>%
      mutate(cat_ftw1 = recode(cat_ftw1,
        negligible = "Negligible",
        low = "Low",
        moderate = "Moderate",
        high = "High"
      ))
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
      labs(#x = "True pest prevalence<br><span style='font-size:16pt;'>(pests/ha)</span>",
           x = "pests/ha",
           y = "Trap Catch (PTW)") +
      theme_bw() +
      theme(text=element_text(size=20),
            #axis.title.x = element_markdown(),
            #axis.title.y = element_markdown()
            )

    # Boxplot
    p2 <- p_df %>%
      ggplot(aes(x = cat_ftw1, y = pests_per_ha, fill = cat_ftw1)) +
      geom_boxplot(outlier.alpha = .01, outlier.size = 1) +
      #scale_color_bright()+
      #scale_fill_bright() +
      scale_x_discrete(labels = c("Negligible\n(<0.01 PTW)",
                                  "Low\n(0.01-1 PTW)",
                                  "Moderate\n(1-10 PTW)",
                                  "High\n(10-100 PTW)")) +
      labs(#y = "True pest prevalence<br><span style='font-size:16pt;'>(pests/ha)</span>",
           y = "pests/ha",
           x = "Trap Catch (PTW)") +
      theme_bw() +
      theme(text=element_text(size=20),
            #axis.title.x = element_markdown(),
            #axis.title.y = element_markdown(),
            legend.position = "none") +
      labs(fill = "Threshold")


    # Arrange plots side by side
    gridExtra::grid.arrange(p2, p1, nrow = 2)
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


  ###################
  # COMPARISON PAGE #
  ###################

  observe({
    if (!is.null(input$comp_num_clust_1)) {
      shinyBS::addTooltip(session, "comp_num_clust_1", cluster_tt, placement = "top")
    }
  })

  observe({
    if (!is.null(input$comp_num_clust_2)) {
      shinyBS::addTooltip(session, "comp_num_clust_2", cluster_tt, placement = "top")
    }
  })

  # Comparison page - Dynamic cluster inputs
  output$comp_input_clusters_1 <- renderUI({
    req(input$comp_spatial_1 == "clustered")
    selectInput("comp_num_clust_1", "Number of Clusters", c(1, 2, 3))
  })

  output$comp_input_clusters_2 <- renderUI({
    req(input$comp_spatial_2 == "clustered")
    selectInput("comp_num_clust_2", "Number of Clusters", c(1, 2, 3))
  })

  # Dynamically update step_size for scenario 1
  observeEvent(input$comp_spatial_1, {
    if (input$comp_spatial_1 == "random") {
      updateSelectInput(session, "comp_step_size_1",
                        choices = c("5 meter" = 5,
                                    "20 meters" = 20,
                                    "43 meters" = 43,
                                    "50 meters" = 50,
                                    "62.6 meters" = 62.6),
                        selected = 43)
    } else {
      updateSelectInput(session, "comp_step_size_1",
                        choices = c("3 meter" = 3,
                                    "7 meter" = 7,
                                    "20 meter" = 20),
                        selected = 3)
    }
    shinyBS::addTooltip(session, "comp_step_size_1", step_size_tt, placement = "top")
  })

  # Dynamically update step_size for scenario 2
  observeEvent(input$comp_spatial_2, {
    if (input$comp_spatial_2 == "random") {
      updateSelectInput(session, "comp_step_size_2",
                        choices = c("5 meter" = 5,
                                    "20 meters" = 20,
                                    "43 meters" = 43,
                                    "50 meters" = 50,
                                    "62.6 meters" = 62.6),
                        selected = 43)
    } else {
      updateSelectInput(session, "comp_step_size_2",
                        choices = c("3 meter" = 3,
                                    "7 meter" = 7,
                                    "20 meter" = 20),
                        selected = 3)
    }
    shinyBS::addTooltip(session, "comp_step_size_2", step_size_tt, placement = "top")
  })

  # Filtered data for scenario 1
  filtered_df_comp_1 <- reactive({
    df_arrow <- shiny_app_df

    if (input$comp_spatial_1 == "random") {
      df_arrow <- df_arrow %>% filter(is.na(num_clust))
    } else {
      this_clust <- as.numeric(input$comp_num_clust_1)
      df_arrow <- df_arrow %>% filter(num_clust == this_clust)
    }

    df_arrow <- df_arrow %>%
      filter(
        n_traps == as.numeric(input$comp_num_traps_1),
        lure_attract == as.numeric(input$comp_lure_attract_1),
        step_size == as.numeric(input$comp_step_size_1)
      )

    df_r <- collect(df_arrow)

    df_r %>%
      mutate(
        pests_per_ha = num_pests / 10,
        cat_ftw1 = ifelse(cat_ftw1 == "very low", "low", cat_ftw1),
        cat_ftw1 = factor(cat_ftw1, levels = c("negligible", "low", "moderate", "high"))
      ) %>%
      mutate(cat_ftw1 = recode(cat_ftw1,
                               negligible = "Negligible",
                               low = "Low",
                               moderate = "Moderate",
                               high = "High"
      ))
  }) %>%
    bindCache(input$comp_num_traps_1, input$comp_lure_attract_1, input$comp_step_size_1,
              input$comp_spatial_1, input$comp_num_clust_1)

  # Filtered data for scenario 2
  filtered_df_comp_2 <- reactive({
    df_arrow <- shiny_app_df

    if (input$comp_spatial_2 == "random") {
      df_arrow <- df_arrow %>% filter(is.na(num_clust))
    } else {
      this_clust <- as.numeric(input$comp_num_clust_2)
      df_arrow <- df_arrow %>% filter(num_clust == this_clust)
    }

    df_arrow <- df_arrow %>%
      filter(
        n_traps == as.numeric(input$comp_num_traps_2),
        lure_attract == as.numeric(input$comp_lure_attract_2),
        step_size == as.numeric(input$comp_step_size_2)
      )

    df_r <- collect(df_arrow)

    df_r %>%
      mutate(
        pests_per_ha = num_pests / 10,
        cat_ftw1 = ifelse(cat_ftw1 == "very low", "low", cat_ftw1),
        cat_ftw1 = factor(cat_ftw1, levels = c("negligible", "low", "moderate", "high"))
      ) %>%
      mutate(cat_ftw1 = recode(cat_ftw1,
                               negligible = "Negligible",
                               low = "Low",
                               moderate = "Moderate",
                               high = "High"
      ))
  }) %>%
    bindCache(input$comp_num_traps_2, input$comp_lure_attract_2, input$comp_step_size_2,
              input$comp_spatial_2, input$comp_num_clust_2)

  # Combined plot for both scenarios (same as before)
  output$comp_plot_combined <- renderPlot({
    p_df1 <- filtered_df_comp_1()
    p_df2 <- filtered_df_comp_2()

    if (nrow(p_df1) == 0 || nrow(p_df2) == 0) {
      plot.new()
      text(0.5, 0.5, "No data for selected filters", cex = 1.5)
      return()
    }

    # Prepare data for line plot
    line_data_1 <- p_df1 %>%
      group_by(pests_per_ha) %>%
      summarise(
        ftw1_mn = mean(ftw1),
        ftw1_lb = quantile(ftw1, 0.05),
        ftw1_ub = quantile(ftw1, 0.95),
        .groups = "drop"
      ) %>%
      mutate(scenario = "Scenario 1")

    line_data_2 <- p_df2 %>%
      group_by(pests_per_ha) %>%
      summarise(
        ftw1_mn = mean(ftw1),
        ftw1_lb = quantile(ftw1, 0.05),
        ftw1_ub = quantile(ftw1, 0.95),
        .groups = "drop"
      ) %>%
      mutate(scenario = "Scenario 2")

    line_data <- bind_rows(line_data_1, line_data_2)

    # Line + ribbon plot
    p1 <- ggplot(line_data, aes(x = pests_per_ha, y = ftw1_mn, color = scenario, fill = scenario)) +
      geom_line(size = 2) +
      geom_ribbon(aes(ymin = ftw1_lb, ymax = ftw1_ub), alpha = 0.2, color = NA) +
      labs(#x = "True pest prevalence<br><span style='font-size:16pt;'>(pests/ha)</span>",
           x = "pests/ha",
           y = "Trap Catch (PTW)",
           color = "Scenario",
           fill = "Scenario") +
      #scale_color_bright() +
      #scale_fill_bright() +
      theme_bw() +
      theme(text = element_text(size = 20),
            #axis.title.x = element_markdown(),
            #axis.title.y = element_markdown(),
            legend.position = "top")

    # Prepare data for boxplot
    box_data_1 <- p_df1 %>%
      mutate(scenario = "Scenario 1")

    box_data_2 <- p_df2 %>%
      mutate(scenario = "Scenario 2")

    box_data <- bind_rows(box_data_1, box_data_2)

    # Boxplot
    p2 <- ggplot(box_data, aes(x = cat_ftw1, y = pests_per_ha, fill = scenario)) +
      geom_boxplot(outlier.alpha = .01, outlier.size = .8, position = position_dodge(width = 0.8)) +
      #scale_fill_bright() +
      scale_x_discrete(labels = c("Negligible\n(<0.01 PTW)",
                                  "Low\n(0.01-1 PTW)",
                                  "Moderate\n(1-10 PTW)",
                                  "High\n(10-100 PTW)")) +
      labs(x = "Threshold Category",
           #y = "True pest prevalence<br><span style='font-size:16pt;'>(pests/ha)</span>",
           y = "pests/ha",
           fill = "Scenario") +
      theme_bw() +
      theme(text = element_text(size = 20),
            #axis.title.x = element_markdown(),
            #axis.title.y = element_markdown(),
            legend.position = "top")

    gridExtra::grid.arrange(p1, p2, nrow = 2)
  })

  # Table for scenario 1
  output$comp_values_table_1 <- renderDT({
    table_df <- filtered_df_comp_1() %>%
      group_by(cat_ftw1) %>%
      summarise(
        val = quantile(pests_per_ha, input$comp_prob_perc / 100),
        .groups = "drop"
      ) %>%
      arrange(cat_ftw1) %>%
      mutate(val = paste0(round(val, 1), " pests/ha"))

    colnames(table_df) <- c("Category", paste0(input$comp_prob_perc, "% Quantile"))

    datatable(table_df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })

  # Table for scenario 2
  output$comp_values_table_2 <- renderDT({
    table_df <- filtered_df_comp_2() %>%
      group_by(cat_ftw1) %>%
      summarise(
        val = quantile(pests_per_ha, input$comp_prob_perc / 100),
        .groups = "drop"
      ) %>%
      arrange(cat_ftw1) %>%
      mutate(val = paste0(round(val, 1), " pests/ha"))

    colnames(table_df) <- c("Category", paste0(input$comp_prob_perc, "% Quantile"))

    datatable(table_df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
}

################
# Run the app  #
################

shinyApp(ui = ui, server = server)
