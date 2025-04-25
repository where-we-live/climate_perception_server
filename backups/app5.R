library(shiny)
library(shinyjs)
library(shinycssloaders)
library(tigris)
library(dplyr)
library(ggplot2)

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
ui <- navbarPage(
  title = div(
    class = "d-flex align-items-center",
    style = "padding-left: 15px; margin-top: 5px;",
    tags$img(
      src = "cropped-haclab_logo.001-e1730319579794.png",
      height = "30px",
      style = "margin-right: 10px;"
    ),
    span(
      "Climate Change Perception Analysis",
      style = "font-size: 18px; font-weight: bold; line-height: 30px;"
    )
  ),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabPanel("Overview",
           h3("Climate Change Perception Analysis"),
           p("Use the tabs above to navigate.")
  ),
  tabPanel("Climate Opinion Maps",
           sidebarLayout(
             sidebarPanel(
               selectInput("year_input1", "Choose a year:", choices = available_years),
               selectInput("var_input1", "Select variable(s):", choices = available_variables, multiple = TRUE),
               actionButton("run1", "Run Plot"),
               textOutput("status1")
             ),
             mainPanel(
               uiOutput("plot1_ui"),
               uiOutput("download1_ui")
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
               uiOutput("plot2_ui"),
               uiOutput("variation_plots_ui"),
               uiOutput("download2_ui")
             )
           )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  plot1_triggered <- reactiveVal(FALSE)
  plot2_triggered <- reactiveVal(FALSE)
  plot2_ready <- reactiveVal(FALSE)
  
  observeEvent(input$state_input2, {
    plot2_triggered(FALSE)
    plot2_ready(FALSE)
  })
  
  observeEvent(input$var_input2, {
    plot2_triggered(FALSE)
    plot2_ready(FALSE)
  })
  
  observeEvent(input$run2, {
    plot2_triggered(TRUE)
    plot2_ready(FALSE)
    shinyjs::disable("run2")
    output$status2 <- renderText("Running...")
    
    output$plot2 <- renderPlot({
      req(plot2_triggered())
      isolate({
        tryCatch({
          plot <- perception_analysis_multi_state_boxplot(input$var_input2, input$state_input2)
          shinyjs::enable("run2")
          output$status2 <- renderText("Done.")
          plot2_ready(TRUE)
          print(plot)
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
    
    output$variation_plots_ui <- renderUI({
      req(plot2_ready())
      req(input$state_input2, input$var_input2)
      plot_output_list <- lapply(input$state_input2, function(state) {
        plotname <- paste0("variation_plot_", gsub(" ", "_", state))
        downloadname <- paste0("download_variation_", gsub(" ", "_", state))
        tagList(
          h4(paste("Histogram for", state)),
          fluidRow(
            column(12, plotOutput(plotname)),
            column(12, downloadButton(downloadname, paste("Download Histogram -", state)))
          )
        )
      })
      do.call(tagList, plot_output_list)
    })
    
    lapply(input$state_input2, function(state) {
      plotname <- paste0("variation_plot_", gsub(" ", "_", state))
      output[[plotname]] <- renderPlot({
        req(plot2_triggered())
        isolate({
          perception_analysis_annual_variation(input$var_input2, state)
        })
      })
    })
    
    lapply(input$state_input2, function(state) {
      downloadname <- paste0("download_variation_", gsub(" ", "_", state))
      output[[downloadname]] <- downloadHandler(
        filename = function() {
          paste0("histogram_", gsub(" ", "_", state), "_", Sys.Date(), ".png")
        },
        contentType = "image/png",
        content = function(file) {
          plot <- perception_analysis_annual_variation(input$var_input2, state)
          png(file, width = 2400, height = 1400, res = 300)
          print(plot)
          dev.off()
        }
      )
    })
  })
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
