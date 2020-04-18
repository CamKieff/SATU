# much of this data is from the blog post Plotting GPS Tracks with R https://rpubs.com/ials2un/gpx1
# originally written by Sascha Wolfer

library(XML)
library(OpenStreetMap)
library(lubridate)
library(ggmap)
library(ggplot2)
library(raster)
library(sp)
library(dplyr)

# ----------------------------------- Import Data ------------------------------------------

GPXpath <- "./strava_archive/activities/" # SET TO PATH FOR STRAVA ACTIVITIES FOLDER

# function to shift a dataframe up (or down)
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

options(digits=10)

import_GPX_data <- function(GPXpath){
  # Create a list of files in the activities
  GPX_list <- list.files(path = GPXpath)
  
  geodf_list <- vector("list", length=length(GPX_list))
  
  for(i in 1:length(GPX_list)){
    pfile <- htmlTreeParse(file = paste0(GPXpath, GPX_list[i]), error = function(...) {
    }, useInternalNodes = T)
    
    elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
    times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
    coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
    
    lats <- as.numeric(coords["lat",])
    lons <- as.numeric(coords["lon",])
    
    geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
    geodf$lat.p1 <- shift.vec(geodf$lat, -1)
    geodf$lon.p1 <- shift.vec(geodf$lon, -1)
    
    # Transform the column ‘time’ so that R knows how to interpret it.
    geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")
    # Shift the time vector, too.
    geodf$time.p1 <- shift.vec(geodf$time, -1)
    # Calculate the number of seconds between two positions.
    geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))
    
    geodf$time <- as.Date(geodf$time)
    geodf$time.p1 <- NULL
    geodf$file <- GPX_list[i]
    
    geodf_list[[i]] <- as.data.frame(geodf)
  }
  
  geodf_all <- bind_rows(geodf_list)
  
  geodf_all$dist.to.prev <- apply(geodf_all, 1, FUN = function (row) {
    pointDistance(c(as.numeric(row["lon.p1"]),
                    as.numeric(row["lat.p1"])),
                  c(as.numeric(row["lon"]), as.numeric(row["lat"])),
                  lonlat = T)
  })
  
  return(geodf_all)
}

import_result <- import_GPX_data((GPXpath = GPXpath))

# ------------------------------- Plotting ---------------------------------------------

# SET LAT AND LON FOR TARGET MAP AREA. This may take some trial and errror

# rough lat/lon boundaries filtered in data set
geodf_DC <- import_result %>% filter(lat < 40 & lat > 38 & lon > -78)

#precise lat/lon boundaries for plotting
lat <- c(38.86, 38.94)
lon <- c(-76.975, -77.075)

bbox <- make_bbox(lon,lat) # makes bounding box

# *********** try to alternative zoom between 10 and 15 ****************
b1 <- get_map(bbox, maptype="hybrid", zoom = 15, source = "google", force = TRUE)

#plot map with elevation as color gradient
p1 <- ggmap(b1) + geom_path(data = geodf_DC, 
                       aes(lon,lat,group = file), col = "red", size=1, alpha=0.2) +
  labs(x = "Longitude", y = "Latitude",
       title="Cameron's Run Density Throughout Washington D.C.") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.line = element_blank(), panel.border = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        plot.title = element_text(size = 16), element_blank(),
        legend.title = element_blank(), legend.position = "none")

#ggsave("Rplot1.svg", plot = p1)
ggsave("Rplot2.png", plot = p1, scale = 2)

# --------------------------------- other data calculations ---------------------------

# Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
geodf_all$speed.m.per.sec <- geodf_all$dist.to.prev / geodf_all$time.diff.to.prev
geodf_all$speed.km.per.h <- geodf_all$speed.m.per.sec * 3.6
geodf_all$speed.km.per.h <- ifelse(is.na(geodf_all$speed.km.per.h), 0, geodf_all$speed.km.per.h)
geodf_all$lowess.speed <- lowess(geodf_all$speed.km.per.h, f = 0.2)$y
geodf_all$lowess.ele <- lowess(geodf_all$ele, f = 0.2)$y
