perception_analysis_state_timeseries_plot <- function(variable_name, state_name) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  
  # Load and fix columns
  yale_2024 <- read.csv("./data/YCOM_2024_publicdata.csv", fileEncoding = "latin1")
  yale_2024$FIPS <- str_pad(yale_2024$geoid, 5, pad = "0")
  
  yale_2024 <- subset(yale_2024, geotype == "county")
  
  yale_2024 <- yale_2024 %>%
    separate(geoname, into = c("county", "state"), sep = ",", remove = FALSE) %>%
    mutate(
      county = str_replace(str_trim(county), "\\s+[Cc]ounty$", ""),
      state = str_trim(state)
    )
  
  # Year columns
  year_cols <- as.character(2010:2024)
  
  # Filter by STATE_NAME and variable
  yale_state <- yale_2024 %>%
    filter(state == state_name, variable == variable_name)
  
  if (nrow(yale_state) == 0) {
    plot.new()
    title("No data available for this state and variable.")
    return(invisible())
  }
  
  names(yale_state)[7:21] <- c(2010:2024)
  
  # Pivot to long format
  df_long <- yale_state %>%
    select(state, county, all_of(year_cols)) %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "year",
      values_to = "value"
    ) %>%
    mutate(year = as.numeric(year))
  
  # Plot
  ggplot(df_long, aes(x = year, y = value, group = county, color = county)) +
    geom_line(alpha = 0.5, linewidth = 0.6, show.legend = FALSE) +
    labs(
      title = paste("Time Series of", variable_name, "in", state_name),
      x = "Year", y = "Value"
    ) +
    theme_minimal()
  
}

