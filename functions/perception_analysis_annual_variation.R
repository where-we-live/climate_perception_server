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

