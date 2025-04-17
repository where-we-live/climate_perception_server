library(shiny)

# ---- Define your plot functions here ----
# Each function takes two inputs: year and variable(s)




perception_analysis_county_timeseries_plot <- function(variable_name, county_fips) {
  
  library(stringr)
  library(sf)
  library(sp)
  
  yale_2024 <- read.csv("./data/YCOM_2024_publicdata.csv")
  yale_2024$FIPS<- str_pad(yale_2024$geoid, 5, pad = "0")
  
  library(dplyr)
  
  yale_tm <- yale_2024 %>% filter(FIPS == county_fips, variable == variable_name)
  yale_tm <- yale_tm[c(2,13:19)]
  colnames(yale_tm) <- c("name", "2018","2019","2020","2021","2022","2023", "2024")
  
  library(tidyr)
  library(dplyr)
  
  df_long <- yale_tm %>%
    pivot_longer(
      cols = -name,
      names_to = "year",
      values_to = "value"
    ) %>%
    mutate(year = as.numeric(year))  # ensure year is numeric for plotting
  
  
  
  library(ggplot2)

  map <- ggplot(df_long, aes(x = year, y = value)) +
    geom_line() +
    geom_point() +
    labs(title = "Time Series Plot",
         x = "Year",
         y = "Value") + theme_minimal()
  return(map)
}


perception_analysis_county_plot <- function(year, variable_name) {
  
  
  library(stringr)
  library(sf)
  library(sp)
  
  yale_2024 <- read.csv("./data/YCOM_2024_publicdata.csv")
  yale_2024$FIPS<- str_pad(yale_2024$geoid, 5, pad = "0")
  yale_2024_county <- subset(yale_2024, geotype == "county")
  names(yale_2024_county)[13:19] <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
  yale_2024_county <- yale_2024_county[-c(5:12)]
  
  yale_2024_county_variable <- yale_2024_county %>% filter(variable == variable_name)
  yale_2024_county_variable2 <- cbind(yale_2024_county_variable[c(1:4)],yale_2024_county_variable[[year]])
  names(yale_2024_county_variable2)[5] <- "year"
  yale_2024_county_variable2$geoid<- str_pad(yale_2024_county_variable2$geoid, 5, pad = "0")
  names(yale_2024_county_variable2)[1] <- "FIPS"
  
  counties <- sf::read_sf("./geodata/UScounties_conus.shp")
  counties <- as_Spatial(counties)
  
  states <- sf::read_sf("./geodata/states_conus.shp")
  states <- as_Spatial(states)
  
  states <- subset(states, STATE_NAME != "Alaska")
  states <- subset(states, STATE_NAME != "Hawaii")
  states <- subset(states, STATE_NAME != "District of Columbia")
  
  #yale_2023_counties <- merge(counties, yale_2023_county_variable, by="FIPS", all.x=TRUE, duplicateGeoms = TRUE)
  yale_2024_counties <- merge(counties, yale_2024_county_variable2, by="FIPS", all.x=TRUE, duplicateGeoms = TRUE)
  
  my_palette <- adjustcolor(colorRampPalette(c("blue","white", "red"))(100), alpha.f = 0.75)
  
  spplot(yale_2024_counties, 
         "year", 
         main = paste("Map of ", variable_name, " for ", year, sep=""),
         col.regions = my_palette, # Change color palette as needed
         at = pretty(yale_2024_counties@data$year, n = 10)) # breaks
  
}


perception_analysis_annual_variation <- function(variable_name) {
  
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  
  yale_2024 <- read.csv("./data/YCOM_2024_publicdata.csv")
  yale_2024$FIPS<- str_pad(yale_2024$geoid, 5, pad = "0")
  yale_2024_county <- subset(yale_2024, geotype == "county")
  names(yale_2024_county)[13:19] <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
  yale_2024_county <- yale_2024_county[-c(5:12)]
  
  y3 <- subset(yale_2024_county, variable == variable_name)
  
  df_long <- y3 %>%
    pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(year))  # make year numeric for gradient coloring
  
  ggplot(df_long, aes(x = value, color = year, group = year)) +
    geom_freqpoly(binwidth = 5, size = .5) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Value", y = "Number of Counties", color = "Year",
         title = "County Value Distributions by Year") +
    theme_minimal()
  
}


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

ui <- navbarPage("My App with Tabs",
                 tabPanel("Home", 
                          h2("This is the home tab")
                 ),
                 tabPanel("Plot", 
                          plotOutput("plot")
                 ),
                 tabPanel("Table",
                          tableOutput("table")
                 )
)



ui <- fluidPage(
  titlePanel("Climate Opinion"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_choice", "Choose a plot:", choices = names(plot_functions)),
      selectInput("year_input", "Choose a year:", choices = available_years),
      selectInput("state_input", "Choose a state:", choices = unique(all_counties$STATE_NAME)),
      selectInput("county_input", "Choose a county:", choices = unique(all_counties$NAME)),
      selectInput("var_input", "Select variable(s):",
                  choices = available_variables,
                  selected = available_variables[1],
                  multiple = TRUE)
    ),
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  output$main_plot <- renderPlot({
    selected_function <- plot_functions[[input$plot_choice]]
    selected_function(input$year_input, input$var_input)
  })
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
