perception_analysis_statewise_annual_variation <- function(variable_name, state_name) {
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  
  yale_2024 <- read.csv("./data/YCOM_2024_publicdata.csv", fileEncoding = "latin1")
  yale_2024$FIPS <- str_pad(yale_2024$geoid, 5, pad = "0")
  yale_2024 <- subset(yale_2024, geotype == "county")
  names(yale_2024)[13:19] <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
  yale_2024 <- yale_2024[-c(5:12)]
  
  y3 <- subset(yale_2024, variable == variable_name & state == state_name)
  
  df_long <- y3 %>%
    pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(year))
  
  ggplot(df_long, aes(x = value, color = year, group = year)) +
    geom_freqpoly(binwidth = 5, size = .5) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Value", y = "Number of Counties", color = "Year",
         title = paste("County Value Distributions by Year:", state_name)) +
    theme_minimal()
}
