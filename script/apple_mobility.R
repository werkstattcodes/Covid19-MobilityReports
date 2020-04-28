# packages ----------------------------------------------------------------

library(tidyverse)
library(hrbrthemes)
extrafont::loadfonts(device = "win", quiet = T)
library(jsonlite)


# https://www.apple.com/covid19/mobility
# The CSV file and charts on this site show a relative volume of directions requests per country/region or city compared to a baseline volume on January 13th, 2020.



# import csv --------------------------------------------------------------
file_link <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/2006HotfixDev9/v1/en-us/applemobilitytrends.json"

li_apple <- jsonlite::fromJSON(file_link, simplifyVector = T) 
df_AT_apple <- map_dfr(li_apple, "Austria")
