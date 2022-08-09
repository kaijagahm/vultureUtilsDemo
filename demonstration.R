# Demonstration script, created for the vulture team meeting on 2022-08-08 to show Orr, Nili, Marta, and Noa.
library(tidyverse)

# Define key parameter values ---------------------------------------------
distThresholdFeeding <- 50 # distance at which vultures are considered interacting for a co-feeding network, in meters
distThresholdFlight <- 1000 # distance at which vultures are considered interacting for a co-flight network, in meters.
consecThreshold <- 2 # minimal number of co-occurrences for considering a viable pair
inIsraelThreshold <- 0.33 # proportion of days tracked that must fall in Israel
roostBuffer <- 50 # buffer around roosting polygons (in meters)

# Get Movebank login credentials ------------------------------------------
load("movebankCredentials/pw.Rda")
MB.LoginObject <- movebankLogin(username = 'kaijagahm', password = pw)
rm(pw)

# Get some data and save it -----------------------------------------------
demoData <- vultureUtils::downloadVultures(loginObject = MB.LoginObject,
                                           extraSensors = F,
                                           removeDup = T,
                                           dateTimeStartUTC = as.POSIXct("2021-12-01 00:00"),
                                           dateTimeEndUTC = as.POSIXct("2022-06-01 00:00"))
save(demoData, file = "data/demoData.Rda")
load("data/demoData.Rda")

# Basic data cleaning -----------------------------------------------------
# convert to a data frame
demoData <- methods::as(demoData, "data.frame")
# remove variables we don't need
demoData <- vultureUtils::removeUnnecessaryVars(demoData)
# add a dateOnly column
demoData$dateOnly <- as.Date(as.character(demoData$timestamp))

# Get Israel mask and roost polygons --------------------------------------
# Get Israel mask
israelMask <- sf::st_read("data/maskIsrael.kml")

# Import roost polygons
roostPolygons <- sf::st_read("data/AllRoostPolygons.kml", quiet = TRUE) %>%
  sf::st_transform("WGS84")

# Buffer the roosts by the buffer distance
roostPolygons <- vultureUtils::convertAndBuffer(obj = roostPolygons, dist = roostBuffer)

# Make feeding edge list --------------------------------------------------
feedingEdges <- vultureUtils::getFeedingEdges(dataset = demoData, 
                                              mask = israelMask, 
                                              roostPolygons = roostPolygons, 
                                              roostBuffer = roostBuffer, 
                                              inMaskThreshold = inIsraelThreshold,
                                              consecThreshold = consecThreshold,
                                              distThreshold = distThresholdFeeding)

# Make flight edge list ---------------------------------------------------
flightEdges <- vultureUtils::getFlightEdges(dataset = demoData, 
                                              mask = israelMask, 
                                              roostPolygons = roostPolygons, 
                                              roostBuffer = roostBuffer, 
                                              inMaskThreshold = inIsraelThreshold,
                                              consecThreshold = consecThreshold,
                                              distThreshold = distThresholdFlight)

# Make networks over a range of intervals ---------------------------------
intervals <- paste(seq(1, 30, by = 5), "days")
feedingNetworks <- lapply(intervals, function(x){
  vultureUtils::makeGraphs(edges = feedingEdges, interval = x,
                           weighted = FALSE,
                           allVertices = TRUE)$graphs
})

flightNetworks <- lapply(intervals, function(x){
  vultureUtils::makeGraphs(edges = flightEdges, interval = x,
                           weighted = FALSE,
                           allVertices = TRUE)$graphs
})

# How does network density change over time? ------------------------------
# Compile the density information for feeding and flight
densFun <- function(.x, .y){
  lapply(.x, igraph::edge_density) %>% 
    unlist() %>% as.data.frame() %>% setNames(., "density") %>%
    mutate(earlyDate = row.names(.),
           interval = factor(.y, levels = .y),
           earlyDate = lubridate::ymd(earlyDate))
}

densitiesFeeding <- map2(.x = feedingNetworks, .y = intervals, .f = densFun) %>% 
  data.table::rbindlist() %>%
  mutate(type = "feeding")
densitiesFlight <- map2(.x = flightNetworks, .y = intervals, .f = densFun) %>% 
  data.table::rbindlist() %>%
  mutate(type = "flight")

# Combine the two
densities <- bind_rows(densitiesFeeding, densitiesFlight)

# Visualize the network density over time
densities %>%
  ggplot(aes(x = earlyDate, y = density, col = interval))+
  geom_point(alpha = 0.5, aes(pch = type))+
  geom_smooth(se = FALSE, aes(lty = type))+
  theme_minimal()+
  scale_color_viridis_d()+
  facet_wrap(~interval)+
  ylab("Network Density")+
  xlab("Start Date")

# Make some GIFS,  for fun ------------------------------------------------
# Let's choose the 6-day window, just for example
feedingNetworks6 <- feedingNetworks[[2]]
flightNetworks6 <- flightNetworks[[2]]

feedingPlotsFixed <- vultureUtils::plotGraphs(feedingNetworks6, coords = "fixed")
flightPlotsFixed <- vultureUtils::plotGraphs(flightNetworks6, coords = "fixed")

vultureUtils::makeGIF(feedingPlotsFixed, fileName = "testgif_feeding.gif", interval = 0.2)
vultureUtils::makeGIF(flightPlotsFixed, fileName = "testgif_flight.gif", interval = 0.2)

