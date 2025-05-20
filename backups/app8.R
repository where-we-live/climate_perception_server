library(shiny)
library(shinyjs)
library(shinycssloaders)
library(tigris)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)

source("perception_analysis_multi_state_boxplot.R")
source("perception_analysis_county_plot.R")
source("perception_analysis_annual_variation.R")

plot_functions <- list(
  "Map" = perception_analysis_county_plot,
  "Histogram" = perception_analysis_annual_variation
)

all_counties <- counties(cb = TRUE)
available_years <- 2018:2024

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

ui <- navbarPage(
  title = "Climate Change Perception Analysis",
  useShinyjs(),
  tabPanel("Overview", h3("Climate Change Perception Analysis"), p("Use the tabs above to navigate.")),
  tabPanel("Climate Opinion Maps",
           sidebarLayout(
             sidebarPanel(
               selectInput("year_input1", "Choose a year:", choices = available_years),
               selectInput("var_input1", "Select variable(s):", choices = available_variables, multiple = TRUE),
               actionButton("run1", "Run Plot"),
               textOutput("status1")
             ),
             mainPanel(uiOutput("plot1_ui"))
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
             mainPanel(uiOutput("plot2_ui"), uiOutput("variation_plots_ui"))
           )
  ),
  tabPanel("Climate Data",
           sidebarLayout(
             sidebarPanel(
               selectInput("clim_var", "Climate Variable:",
                           choices = c("aet", "def", "PDSI", "pet", "ppt", "q", "soil", "srad",
                                       "swe", "tmax", "tmin", "vap", "vpd", "ws")),
               selectInput("clim_year", "Select Year:", choices = 1993:2024),
               selectInput("clim_month", "Select Month:", choices = 1:12),
               actionButton("run_climate", "Run"),
               textOutput("status_climate")
             ),
             mainPanel(uiOutput("map_ui"), uiOutput("trend_ui"), uiOutput("download_ui"))
           )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # --- Climate Opinion Maps Tab ---
  plot1_triggered <- reactiveVal(FALSE)
  observeEvent(input$run1, {
    plot1_triggered(TRUE)
    shinyjs::disable("run1")
    output$status1 <- renderText("Running...")
    
    output$plot1 <- renderPlot({
      req(plot1_triggered())
      tryCatch({
        plot <- perception_analysis_county_plot(input$year_input1, input$var_input1)
        shinyjs::enable("run1")
        output$status1 <- renderText("Done.")
        plot
      }, error = function(e) {
        shinyjs::enable("run1")
        output$status1 <- renderText("Error: Check input or data")
        plot.new(); title("Plot Error")
        message("Plot error: ", e$message)
      })
    })
  })
  
  output$plot1_ui <- renderUI({
    if (plot1_triggered()) withSpinner(plotOutput("plot1")) else NULL
  })
  
  # --- Climate Opinion County Comparison Tab ---
  plot2_triggered <- reactiveVal(FALSE)
  observeEvent(input$run2, {
    plot2_triggered(TRUE)
    shinyjs::disable("run2")
    output$status2 <- renderText("Running...")
    
    output$plot2 <- renderPlot({
      req(input$state_input2, input$var_input2)
      tryCatch({
        plot <- perception_analysis_multi_state_boxplot(input$var_input2, input$state_input2)
        shinyjs::enable("run2")
        output$status2 <- renderText("Done.")
        plot
      }, error = function(e) {
        shinyjs::enable("run2")
        output$status2 <- renderText("Error: Check input or data")
        plot.new(); title("Plot Error")
        message("Plot error: ", e$message)
      })
    })
  })
  
  output$plot2_ui <- renderUI({
    if (plot2_triggered()) withSpinner(plotOutput("plot2")) else NULL
  })
  
  output$variation_plots_ui <- renderUI({
    req(plot2_triggered())
    req(input$state_input2, input$var_input2)
    
    plot_output_list <- lapply(input$state_input2, function(state) {
      plotname <- paste0("variation_plot_", gsub("\\s", "_", state))
      tagList(
        h4(paste("Histogram for", state)),
        fluidRow(column(12, plotOutput(plotname)))
      )
    })
    do.call(tagList, plot_output_list)
  })
  
  observe({
    req(plot2_triggered())
    lapply(input$state_input2, function(state) {
      plotname <- paste0("variation_plot_", gsub("\\s", "_", state))
      output[[plotname]] <- renderPlot({
        perception_analysis_annual_variation(input$var_input2, state)
      })
    })
  })
  
  # --- Climate Data Tab ---
  climate_triggered <- reactiveVal(FALSE)
  observeEvent(input$run_climate, {
    climate_triggered(TRUE)
    shinyjs::disable("run_climate")
    output$status_climate <- renderText("Running...")
    
    filename <- paste0("data/", input$clim_var, ".csv")
    df <- tryCatch(read.csv(filename, colClasses = c(FIPS = "character")), error = function(e) NULL)
    
    if (is.null(df)) {
      output$status_climate <- renderText("Failed to load data.")
      return()
    }
    
    df$FIPS <- str_pad(df$FIPS, 5, pad = "0")
    counties_sf <- st_read("geodata/UScounties_conus.shp")
    counties_sf$FIPS <- str_pad(as.character(counties_sf$FIPS), 5, pad = "0")
    
    df_map <- left_join(counties_sf, df, by = "FIPS")
    
    output$map_plot <- renderPlot({
      req(climate_triggered())
      isolate({
        tryCatch({
          var_col <- input$clim_var
          plot_df <- df_map %>% filter(year == input$clim_year, month == input$clim_month)
          ggplot(plot_df) +
            geom_sf(aes_string(fill = var_col), color = NA) +
            scale_fill_viridis_c(option = "C", na.value = "gray80") +
            labs(title = paste(toupper(var_col), "for", input$clim_year, "Month", input$clim_month),
                 fill = var_col) +
            theme_minimal()
        }, error = function(e) {
          plot.new(); title("Error rendering map")
        })
      })
    })
    
    output$trend_plot <- renderPlot({
      req(climate_triggered())
      isolate({
        tryCatch({
          var_col <- input$clim_var
          monthly_df <- df %>%
            filter(year == input$clim_year) %>%
            group_by(month) %>%
            summarize(mean_value = mean(.data[[var_col]], na.rm = TRUE))
          
          ggplot(monthly_df, aes(x = month, y = mean_value)) +
            geom_line(color = "blue") +
            geom_point() +
            labs(title = paste("Monthly", var_col, "in", input$clim_year),
                 x = "Month", y = "Mean Value") +
            theme_minimal()
        }, error = function(e) {
          plot.new(); title("Error rendering trend plot")
        })
      })
    })
    
    output$status_climate <- renderText("Done.")
    shinyjs::enable("run_climate")
  })
  
  output$map_ui <- renderUI({
    if (climate_triggered()) withSpinner(plotOutput("map_plot")) else NULL
  })
  
  output$trend_ui <- renderUI({
    if (climate_triggered()) withSpinner(plotOutput("trend_plot")) else NULL
  })
  
  output$download_ui <- renderUI({
    req(climate_triggered())
    downloadButton("download_clim_map_data", "Download Filtered Data")
  })
  
  output$download_clim_map_data <- downloadHandler(
    filename = function() {
      paste0("climate_", input$clim_var, "_", input$clim_year, "_month", input$clim_month, ".csv")
    },
    content = function(file) {
      df <- read.csv(paste0("data/", input$clim_var, ".csv"), colClasses = c(FIPS = "character"))
      df$FIPS <- str_pad(df$FIPS, 5, pad = "0")
      write.csv(df %>% filter(year == input$clim_year, month == input$clim_month), file, row.names = FALSE)
    }
  )
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
