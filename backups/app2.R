library(shiny)

# ---- Define your plot functions here ----
# Each function takes two inputs: year and variable(s)


source("perception_analysis_county_timeseries_plot.R")
source("perception_analysis_county_plot.R")
source("perception_analysis_annual_variation.R")


# ---- Map choices to functions ----
plot_functions <- list(
  "Time Series" = perception_analysis_county_timeseries_plot,
  "Map" = perception_analysis_county_plot,
  "Histogram" = perception_analysis_annual_variation
)

# Dummy values for UI choices (replace with your data-driven values)
library(tigris)
all_counties <- counties(cb = TRUE)
# Get just the FIPS codes
fips_codes <- all_counties$GEOID  # GEOID is the 5-digit FIPS code
# View first few
head(fips_codes)
available_years <- 2018:2024
available_variables <-  c("affectweather", "affectweatherOppose", "citizens", "citizensOppose",
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
  "trustclimscisstOppose", "worried", "worriedOppose"
)




# ---- UI ----

ui <- navbarPage("Climate Change Perception Analysis",
                 
                 
                 tabPanel("Overview",
                          h3("Climate Change Perception Analysis"),
                          p("Use the tabs above to navigate.")
                 ),
                 
                 tabPanel("Climate Opinion Maps",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plot_choice", "Choose a plot:", choices = names(plot_functions)),
                              selectInput("year_input", "Choose a year:", choices = available_years),
                              selectInput("var_input", "Select variable(s):",
                                          choices = available_variables,
                                          selected = available_variables[1],
                                          multiple = TRUE)),
                            mainPanel(
                              actionButton("run1", "Run Plot 1"),
                              plotOutput("plot1")
                            )
                          )
                 ),
                 
                 tabPanel("Climate Opinion County Comparison",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plot_choice", "Choose a plot:", choices = names(plot_functions)),
                              selectInput("state_input", "Choose a state:", choices = unique(all_counties$STATE_NAME)),
                              selectInput("county_input", "Choose a county:", choices = unique(all_counties$NAME)),
                              selectInput("var_input", "Select variable(s):",
                                          choices = available_variables,
                                          selected = available_variables[1],
                                          multiple = TRUE)),
                            mainPanel(
                              plotOutput("main_plot")
                            )
                          )
                 ),
                 
                 tabPanel("Climate Data",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plot_choice", "Choose a plot:", choices = names(plot_functions)),
                              selectInput("state_input", "Choose a state:", choices = unique(all_counties$STATE_NAME)),
                              selectInput("county_input", "Choose a county:", choices = unique(all_counties$NAME)),
                              selectInput("var_input", "Select variable(s):",
                                          choices = available_variables,
                                          selected = available_variables[1],
                                          multiple = TRUE)),
                            mainPanel(
                              plotOutput("main_plot")
                            )
                          )
                 ),
                 
                 tabPanel("PDI",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plot_choice", "Choose a plot:", choices = names(plot_functions)),
                              selectInput("state_input", "Choose a state:", choices = unique(all_counties$STATE_NAME)),
                              selectInput("county_input", "Choose a county:", choices = unique(all_counties$NAME)),
                              selectInput("var_input", "Select variable(s):",
                                          choices = available_variables,
                                          selected = available_variables[1],
                                          multiple = TRUE)),
                            mainPanel(
                              plotOutput("main_plot")
                            )
                          )
                 ),
                 
                 tabPanel("Models",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plot_choice", "Choose a plot:", choices = names(plot_functions)),
                              selectInput("state_input", "Choose a state:", choices = unique(all_counties$STATE_NAME)),
                              selectInput("county_input", "Choose a county:", choices = unique(all_counties$NAME)),
                              selectInput("var_input", "Select variable(s):",
                                          choices = available_variables,
                                          selected = available_variables[1],
                                          multiple = TRUE)),
                            mainPanel(
                              plotOutput("main_plot")
                            )
                          )
                          
                          
                 )
               
)
          

# ---- Server ----
server <- function(input, output, session) {
  
  observeEvent(input$run1, {
  output$plot1 <- renderPlot({
    selected_function <- plot_functions[[input$plot_choice]]
    selected_function(input$year_input, input$var_input)
  })
})
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
