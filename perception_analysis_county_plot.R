perception_analysis_county_plot <- function(year, variable_name) {
  library(stringr)
  library(sf)
  library(sp)
  
  yale_2024 <- read.csv("./data/YCOM_2024_publicdata.csv")
  yale_2024$FIPS <- str_pad(yale_2024$geoid, 5, pad = "0")
  yale_2024_county <- subset(yale_2024, geotype == "county")
  names(yale_2024_county)[13:19] <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
  yale_2024_county <- yale_2024_county[-c(5:12)]
  
  yale_2024_county_variable <- yale_2024_county %>% filter(variable == variable_name)
  yale_2024_county_variable2 <- cbind(yale_2024_county_variable[1:4], yale_2024_county_variable[[year]])
  names(yale_2024_county_variable2)[5] <- "year"
  yale_2024_county_variable2$geoid <- str_pad(yale_2024_county_variable2$geoid, 5, pad = "0")
  names(yale_2024_county_variable2)[1] <- "FIPS"
  
  # 💥 Add this data validity check
  if (nrow(yale_2024_county_variable2) == 0 || all(is.na(yale_2024_county_variable2$year))) {
    stop("No valid data for selected variable and year.")
  }
  
  counties <- sf::read_sf("./geodata/UScounties_conus.shp")
  counties <- as_Spatial(counties)
  
  states <- sf::read_sf("./geodata/states_conus.shp")
  states <- as_Spatial(states)
  states <- subset(states, STATE_NAME != "Alaska" & STATE_NAME != "Hawaii" & STATE_NAME != "District of Columbia")
  
  yale_2024_counties <- merge(counties, yale_2024_county_variable2, by = "FIPS", all.x = TRUE, duplicateGeoms = TRUE)
  
  my_palette <- adjustcolor(colorRampPalette(c("blue", "white", "red"))(100), alpha.f = 0.75)
  
  return(
    spplot(yale_2024_counties,
           "year",
           main = paste("Map of ", variable_name, " for ", year, sep = ""),
           col.regions = my_palette,
           at = pretty(yale_2024_counties@data$year, n = 10))
  )
}
