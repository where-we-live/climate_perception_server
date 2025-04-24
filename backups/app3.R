library(shiny)
library(shinyjs)
library(shinycssloaders)
library(tigris)
library(dplyr)

# ---- Load Plot Functions ----
source("perception_analysis_multi_state_boxplot.R")
source("perception_analysis_county_plot.R")
source("perception_analysis_annual_variation.R")

# ---- Plot Function Mapping ----
plot_functions <- list(
  "Map" = perception_analysis_county_plot,
  "Histogram" = perception_analysis_annual_variation
)

# ---- County boundaries for state options ----
all_counties <- counties(cb = TRUE)
available_years <- 2018:2024

# ---- YCOM Variables ----
available_variables <- c("affectweather", "affectweatherOppose", "citizens", "citizensOppose",
                         "co2limits", "co2limitsOppose", "congress", "congressOppose",
                         "consensus", "consensusOppose", "corporations", "corporationsOppose",
                         "devharm", "devharmOppose", "discuss", "discussOppose",
                         "drillanwr", "drillanwrOppose", "drilloffshore", "drilloffshoreOppose",
                         "exp", "expOppose", "fundrenewables", "fundrenewablesOppose",
                         "futuregen", "futuregenOppose", "generaterenewable", "generaterenewableOppose",
                         "governor", "governorOppose", "gwvoteimp", "gwvoteimpOppose",
                         "happening", "happeningOppose", "harmplants", "harmplantsOppose",
                         "harmus", "harmusOppose", "human", "humanOppose",
                         "important", "localofficials", "localofficialsOppose", "mediaweekly",
                         "mediaweeklyOppose", "personal", "personalOppose", "president",
                         "presidentOppose", "prienv", "prienvOppose", "priority",
                         "priorityOppose", "prioritycleanenergy", "prioritycleanenergyOppose", "rebates",
                         "rebatesOppose", "reducetax", "reducetaxOppose", "regulate",
                         "regulateOppose", "supportrps", "supportrpsOppose", "taxdividend",
                         "taxdividendOppose", "teachgw", "teachgwOppose", "timing",
                         "timingOppose", "transitioneconomy", "transitioneconomyOppose", "trustclimscisst",
                         "trustclimscisstOppose", "worried", "worriedOppose")

# ---- UI ----
ui <- navbarPage("Climate Change Perception Analysis",
                 useShinyjs(),
                 
                 tabPanel("Overview",
                          h3("Climate Change Perception Analysis"),
                          p("Use the tabs above to navigate.")
                 ),
                 
                 tabPanel("Climate Opinion Maps",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plot_choice1", "Choose a plot:", choices = names(plot_functions)),
                              selectInput("year_input1", "Choose a year:", choices = available_years),
                              selectInput("var_input1", "Select variable(s):",
                                          choices = available_variables, multiple = TRUE),
                              actionButton("run1", "Run Plot"),
                              textOutput("status1")
                            ),
                            mainPanel(
                              withSpinner(plotOutput("plot1"))
                            )
                          )
                 ),
                 
                 tabPanel("Climate Opinion County Comparison",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("state_input2", "Choose states:", choices = sort(unique(all_counties$STATE_NAME)), multiple = TRUE),
                              selectInput("var_input2", "Select variable:", choices = available_variables),
                              actionButton("run2", "Run Plot"),
                              textOutput("status2")
                            ),
                            mainPanel(
                              uiOutput("plot2_ui")  # dynamically loaded only after Run is clicked
                            )
                          )
                 ),
                 
                 tabPanel("Climate Data",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plot_choice3", "Choose a plot:", choices = names(plot_functions)),
                              selectInput("year_input3", "Choose a year:", choices = available_years),
                              selectInput("var_input3", "Select variable(s):",
                                          choices = available_variables, multiple = TRUE),
                              actionButton("run3", "Run Plot"),
                              textOutput("status3")
                            ),
                            mainPanel(
                              withSpinner(plotOutput("plot3"))
                            )
                          )
                 ),
                 
                 tabPanel("PDI",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plot_choice4", "Choose a plot:", choices = names(plot_functions)),
                              selectInput("year_input4", "Choose a year:", choices = available_years),
                              selectInput("var_input4", "Select variable(s):",
                                          choices = available_variables, multiple = TRUE),
                              actionButton("run4", "Run Plot"),
                              textOutput("status4")
                            ),
                            mainPanel(
                              withSpinner(plotOutput("plot4"))
                            )
                          )
                 ),
                 
                 tabPanel("Models",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plot_choice5", "Choose a plot:", choices = names(plot_functions)),
                              selectInput("year_input5", "Choose a year:", choices = available_years),
                              selectInput("var_input5", "Select variable(s):",
                                          choices = available_variables, multiple = TRUE),
                              actionButton("run5", "Run Plot"),
                              textOutput("status5")
                            ),
                            mainPanel(
                              withSpinner(plotOutput("plot5"))
                            )
                          )
                 )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # --- Trigger control for plot2 ---
  plot2_triggered <- reactiveVal(FALSE)
  
  # Reset trigger when inputs change
  observeEvent(list(input$state_input2, input$var_input2), {
    plot2_triggered(FALSE)
  })
  
  # Run plot when Run button clicked
  observeEvent(input$run2, {
    plot2_triggered(TRUE)
    shinyjs::disable("run2")
    output$status2 <- renderText("Running...")
    
    output$plot2 <- renderPlot({
      req(plot2_triggered())
      req(input$state_input2, input$var_input2)
      tryCatch({
        plot <- perception_analysis_multi_state_boxplot(input$var_input2, input$state_input2)
        shinyjs::enable("run2")
        output$status2 <- renderText("Done.")
        plot
      }, error = function(e) {
        shinyjs::enable("run2")
        output$status2 <- renderText("Error: Check input or data")
        plot.new()
        title("Plot Error")
        message("Plot error: ", e$message)
      })
    })
  })
  
  output$plot2_ui <- renderUI({
    if (plot2_triggered()) {
      withSpinner(plotOutput("plot2"))
    } else {
      NULL
    }
  })
  
  # ---- Generalized runner for other tabs ----
  run_plot <- function(run_id, plot_id, status_id, choice, year, vars) {
    triggered <- reactiveVal(FALSE)
    
    observeEvent(list(input[[choice]], input[[year]], input[[vars]]), {
      triggered(FALSE)
    })
    
    observeEvent(input[[run_id]], {
      shinyjs::disable(run_id)
      output[[status_id]] <- renderText("Running...")
      triggered(TRUE)
    })
    
    output[[plot_id]] <- renderPlot({
      req(triggered())
      tryCatch({
        selected_function <- plot_functions[[input[[choice]]]]
        plot <- selected_function(input[[year]], input[[vars]])
        shinyjs::enable(run_id)
        output[[status_id]] <- renderText("Done.")
        plot
      }, error = function(e) {
        shinyjs::enable(run_id)
        output[[status_id]] <- renderText("Error: Check input or data")
        plot.new()
        title("Plot Error")
        message("Plot error: ", e$message)
      })
    })
  }
  
  run_plot("run1", "plot1", "status1", "plot_choice1", "year_input1", "var_input1")
  run_plot("run3", "plot3", "status3", "plot_choice3", "year_input3", "var_input3")
  run_plot("run4", "plot4", "status4", "plot_choice4", "year_input4", "var_input4")
  run_plot("run5", "plot5", "status5", "plot_choice5", "year_input5", "var_input5")
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)

