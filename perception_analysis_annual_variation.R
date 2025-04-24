perception_analysis_annual_variation <- function(variable_name, state_name_input) {
  
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  
  yale_2024 <- read.csv("./data/YCOM_2024_publicdata.csv", fileEncoding = "latin1")
  
  yale_2024$FIPS <- str_pad(yale_2024$geoid, 5, pad = "0")
  yale_2024_county <- subset(yale_2024, geotype == "county")
  names(yale_2024_county)[13:19] <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
  yale_2024_county <- yale_2024_county[-c(5:12)]
  
  
  # Extract "county" and "state" from geoname
  yale_2024_county <- yale_2024_county %>%
    separate(geoname, into = c("county", "state"), sep = ",", remove = FALSE) %>%
    mutate(
      county = str_replace(str_trim(county), "\\s+[Cc]ounty$", ""),
      state = str_trim(state)
    )
  
  
  # Fix is here:
  y3 <- subset(yale_2024_county, variable == variable_name & state == state_name_input)
  
  df_long <- y3 %>%
    pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(year))
  
  ggplot(df_long, aes(x = value, color = year, group = year)) +
    geom_freqpoly(binwidth = 5, size = 0.5) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Value", y = "Number of Counties", color = "Year",
         title = paste("County Value Distributions by Year in", state_name_input)) +
    theme_minimal()
}
