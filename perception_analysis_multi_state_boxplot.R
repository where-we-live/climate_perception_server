perception_analysis_multi_state_boxplot <- function(variable_name, state_names) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  
  # Load and fix columns
  yale_2024 <- read.csv("./data/YCOM_2024_publicdata.csv", fileEncoding = "latin1")
  yale_2024$FIPS <- str_pad(yale_2024$geoid, 5, pad = "0")
  yale_2024 <- subset(yale_2024, geotype == "county")
  
  # Extract "county" and "state" from geoname
  yale_2024 <- yale_2024 %>%
    separate(geoname, into = c("county", "state"), sep = ",", remove = FALSE) %>%
    mutate(
      county = str_replace(str_trim(county), "\\s+[Cc]ounty$", ""),
      state = str_trim(state)
    )
  
  # Rename columns 7:21 as year columns
  names(yale_2024)[7:21] <- as.character(2010:2024)
  year_cols <- as.character(2010:2024)
  
  # Filter data
  df <- yale_2024 %>%
    filter(state %in% state_names, variable == variable_name)
  
  if (nrow(df) == 0) {
    plot.new()
    title("No data available for the selected states and variable.")
    return(invisible())
  }
  
  # Reshape to long format
  df_long <- df %>%
    select(state, county, all_of(year_cols)) %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "year",
      values_to = "value"
    ) %>%
    mutate(year = as.numeric(year)) %>%
    group_by(year) %>%
    filter(!all(is.na(value))) %>%  # <- Only change: remove years where all values are NA
    ungroup()
  
  # Boxplot grouped by year, colored by state
  ggplot(df_long, aes(x = factor(year), y = value, fill = state)) +
    geom_boxplot(position = position_dodge(0.8), outlier.color = "red", alpha = 0.6) +
    labs(
      title = paste("County-Level", variable_name, "Distributions by Year and State"),
      x = "Year",
      y = "Value",
      fill = "State"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

