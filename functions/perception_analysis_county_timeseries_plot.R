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
  
  ggplot(df_long, aes(x = year, y = value)) +
    geom_line() +
    geom_point() +
    labs(title = "Time Series Plot",
         x = "Year",
         y = "Value") + theme_minimal()
  
}
