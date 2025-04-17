#variables

#"affectweather"             "affectweatherOppose"       "citizens"                  "citizensOppose"           
#"co2limits"                 "co2limitsOppose"           "congress"                  "congressOppose"           
#"consensus"                 "consensusOppose"           "corporations"              "corporationsOppose"       
#"devharm"                   "devharmOppose"             "discuss"                   "discussOppose"            
#"drillanwr"                 "drillanwrOppose"           "drilloffshore"             "drilloffshoreOppose"      
#"exp"                       "expOppose"                 "fundrenewables"            "fundrenewablesOppose"     
#"futuregen"                 "futuregenOppose"           "generaterenewable"         "generaterenewableOppose"  
#"governor"                  "governorOppose"            "gwvoteimp"                 "gwvoteimpOppose"          
#"happening"                 "happeningOppose"           "harmplants"                "harmplantsOppose"         
#"harmus"                    "harmusOppose"              "human"                     "humanOppose"              
#"important"                 "localofficials"            "localofficialsOppose"      "mediaweekly"              
#"mediaweeklyOppose"         "personal"                  "personalOppose"            "president"                
#"presidentOppose"           "prienv"                    "prienvOppose"              "priority"                 
#"priorityOppose"            "prioritycleanenergy"       "prioritycleanenergyOppose" "rebates"                  
#"rebatesOppose"             "reducetax"                 "reducetaxOppose"           "regulate"                 
#"regulateOppose"            "supportrps"                "supportrpsOppose"          "taxdividend"              
#"taxdividendOppose"         "teachgw"                   "teachgwOppose"             "timing"                   
#"timingOppose"              "transitioneconomy"         "transitioneconomyOppose"   "trustclimscisst"          
#"trustclimscisstOppose"     "worried"                   "worriedOppose"            




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
