
# Tooltips descriptions
num_traps_tt <- "How many traps should be placed in the 10ha site?"
attract_tt <- "Distance from a trap (in metres) where expected value of p(detection) is 65%. "
step_size_tt <- "Average amount distance a pest will travel (in metres) per day."
spatial_tt <- "Should pests be randomly distributed or clustered?"
cluster_tt <- "How many clusters of pests?"
perc_tt <- "What perentile should we calculate for the upper limit?"

# Plot page
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
                   step = 0.1),

    # Tooltips
    bsTooltip("num_traps",
              num_traps_tt,
              placement = "top"),
    bsTooltip("lure_attract",
              attract_tt,
              placement = "top"),
    #bsTooltip("step_size",
    #          step_size_tt,
    #          placement = "top"),
    bsTooltip("spatial",
              spatial_tt,
              placement = "top"),
    bsTooltip("prob_perc",
              perc_tt,
              placement = "top")
    ),

    mainPanel(
      plotOutput("plot", height = "600px", width = "520px"),
      DTOutput("values_table", width = "520px")
    )
  )
}

# Comparison page
view_comparison_page <- function() {
  sidebarLayout(
    sidebarPanel(
      # Shared input
      numericInput("comp_prob_perc",
                   "Percent confidence of pests below",
                   value = 95,
                   min = 1, max = 100,
                   step = 0.1),

      hr(),

      # Scenario 1
      h4("Scenario 1"),
      selectInput("comp_num_traps_1",
                  "Number of traps",
                  c("1 Trap" = 1,
                    "3 Traps" = 3,
                    "5 Traps" = 5,
                    "10 Traps" = 10),
                  selected = 1),
      selectInput("comp_lure_attract_1",
                  "Lure attractiveness",
                  c("7 meters" = 7,
                    "14 meters" = 14,
                    "25 meters" = 25,
                    "36 meters" = 36,
                    "50 meters" = 50),
                  selected = 14),
      selectInput("comp_step_size_1",
                  "Mean step Size",
                  c("5 meter" = 5,
                    "20 meters" = 20,
                    "43 meters" = 43,
                    "50 meters" = 50,
                    "62.6 meters" = 62.6),
                  selected = 43),
      selectInput("comp_spatial_1",
                  "Random or Clustered?",
                  c("Random" = "random",
                    "Clustered" = "clustered"),
                  selected = "random"),
      conditionalPanel(
        condition = "input.comp_spatial_1 == 'clustered'",
        uiOutput("comp_input_clusters_1")
      ),

      hr(),

      # Scenario 2
      h4("Scenario 2"),
      selectInput("comp_num_traps_2",
                  "Number of traps",
                  c("1 Trap" = 1,
                    "3 Traps" = 3,
                    "5 Traps" = 5,
                    "10 Traps" = 10),
                  selected = 3),
      selectInput("comp_lure_attract_2",
                  "Lure attractiveness",
                  c("7 meters" = 7,
                    "14 meters" = 14,
                    "25 meters" = 25,
                    "36 meters" = 36,
                    "50 meters" = 50),
                  selected = 14),
      selectInput("comp_step_size_2",
                  "Mean step Size",
                  c("5 meter" = 5,
                    "20 meters" = 20,
                    "43 meters" = 43,
                    "50 meters" = 50,
                    "62.6 meters" = 62.6),
                  selected = 43),
      selectInput("comp_spatial_2",
                  "Random or Clustered?",
                  c("Random" = "random",
                    "Clustered" = "clustered"),
                  selected = "random"),
      conditionalPanel(
        condition = "input.comp_spatial_2 == 'clustered'",
        uiOutput("comp_input_clusters_2")
      ),

      # Tooltips
      bsTooltip("comp_prob_perc",
                perc_tt,
                placement = "top"),
      bsTooltip("comp_num_traps_1",
                num_traps_tt,
                placement = "top"),
      bsTooltip("comp_lure_attract_1",
                attract_tt,
                placement = "top"),
      #bsTooltip("comp_step_size_1",
      #          step_size_tt,
      #          placement = "top"),
      bsTooltip("comp_spatial_1",
                spatial_tt,
                placement = "top"),
      bsTooltip("comp_num_traps_2",
                num_traps_tt,
                placement = "top"),
      bsTooltip("comp_lure_attract_2",
                attract_tt,
                placement = "top"),
      #bsTooltip("comp_step_size_2",
      #          step_size_tt,
      #          placement = "top"),
      bsTooltip("comp_spatial_2",
                spatial_tt,
                placement = "top")
    ),

    mainPanel(
      plotOutput("comp_plot_combined", height = "600px", width = "520px"),
      fluidRow(
        column(6,
               h4("Scenario 1"),
               DTOutput("comp_values_table_1", width="260px")),
        column(6,
               h4("Scenario 2"),
               DTOutput("comp_values_table_2", width="260px"))
      )
    )
  )
}

# About the site page
view_about_page <- function() {
  tags$div(
    tags$h4("About"),
    "This shiny app is a companion to", tags$a(href="https://research.csiro.au/prs/", "Gladish et al. (2025)."),
    "Users input trap and pest paremters to explore plausible active pest prevelances on a site. The tool allows
    users to undertake their own scenarios using the simulation model published in Gladish et al. (2025).",
    "There are two main pages to this app: ", tags$br(),
    tags$ul(
      tags$li(tags$b("Plots: "), "TODO"),
      tags$li(tags$b("Compare Scenarios: "), "TODO")
    ),
    "Users can specify trap and pest parameters to explore plausible pest prevelane on site: ",
    tags$h6("Trap Parameters"),
    tags$ul(
      tags$li("Number of traps: How many traps placed in the site."),
      tags$li("Lure attractiveness: distance from trap (in metres) where expected value of p(detection) is 65%.  For example,
              suitable lure attractiveness for Mediterranean fruit fly has been estimated tomay be 14m while Oriental fruit fly
              was may be 36m.  Lure attractiveness of 5m is extremely low while 50m is extremely attractive.")
    ),
    tags$h6("Pest Parameters"),
    tags$ul(
      tags$li("Number of pests: how many pests in the outbreak are released for each simulation?"),
      tags$li("Step size of the pest: how far each pest travels per day (in metres) on average. Examples include grapevine moth at 20 m/day,
              Mediterranean fruit fly at 43 m/day, and Oriental fruit fly at 63 m/day. ")
    ),
    tags$h6("Percentile"),
    tags$ul(
      tags$li("Specifies the upper percentile of pests on site.")
    ),
    tags$h4("Use of the App"),
    "CSIRO’s", tags$a(href="https://www.csiro.au/en/about/policies/legal/legal-notice","Legal notice and disclaimer")," and",
    tags$a(href="https://www.csiro.au/en/about/Policies/Privacy","Privacy Policy"), "apply to the use of this App.", tags$br(),
    "Please note: this App is actively being developed and intended for the user to explore the model described in van Klinken et al. (2023).
    The user must make its own assessment of the suitability for its use of the information or material contained in or generated from the App
    for any other purpose and to seek further professional, scientific and technical advice as may be necessary.",
    tags$br(), tags$br(),
    tags$img(src = "csiro-logo.svg", width = "150px", height = "150px")

  )
}


# Reference and acknowledgements page
view_ref_page <- function() {
  tags$div(
    tags$h4("Acknowledgements"),
    "TODO",
    #"We thank the many collaborators from the Australian Department of Agriculture, Water and the Environment, Agriculture Victoria, New South Wales Department of Primary Industries, Western
    #Australian Department of Primary Industries and Regional Development, Hort Innovation and CSIRO for their conceptual contributions.", tags$br(),tags$br(),

    #"The development of this app was supported by CSIRO, Federal and all State Governments with additional funding contributions from CSIRO, the apple and pear levy, Apple and Pear Australia
    #Limited, and Fruit Growers Tasmania. This project is being delivered through Hort Innovation’s Hort Frontiers strategic partnership initiative. Hort Frontiers facilitates collaborative,
    #transformational research and development to support horticulture to 2030, and beyond.", tags$br(),tags$br(),

    #"This project further acknowledges CSIRO Trusted Agrifood Exports Mission.",tags$br(),

    tags$h4("Citation"),
    "TODO",
    #"Please use the following citation for references:", tags$br(),tags$br(),
    #"Gladish, Dan; Van Klinken, Rieks (2023): Web app for: Simulation to investigate site-based monitoring of pest insect species for trade. v1. CSIRO. Service Collection.
    #http://hdl.handle.net/102.100.100/489692?index=1",tags$br(),

    tags$h4("Reference"),
    "TODO",
    #"van Klinken, Rieks D; Gladish, Daniel W; Manoukis, Nicholas C; Caley, Peter; Hill, Matthew P (2023). Simulation to investigate site-based monitoring of pest insect species for trade, Journal of Economic Entomology, toad112. ",tags$a(href="https://doi.org/10.1093/jee/toad112","https://doi.org/10.1093/jee/toad112"),tags$br(),tags$br(),
    #"Gladish, Dan; Hill, Matt; Caley, Peter (2022): trapDetect R Package. v1. CSIRO. Software Collection. ",tags$a(href="https://doi.org/10.25919/mc7t-9q85","https://doi.org/10.25919/mc7t-9q85"),

    tags$h4("Contact"),
    "Dr Dan Gladish, CSIRO Data61, dan.gladish@data61.csiro.au",

    tags$h4("Known Issues"),
    tags$ol(
      tags$li("Tool tip for step size is not working")
    ),
    "Please contact for additional issues or suggestions."
  )
}
