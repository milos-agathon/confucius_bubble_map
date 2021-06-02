################################################################################
#                  Create animated bubble map of China's 
#                  Confucius Institutes
#                  Milos Popovic
#                  2021/5/31
################################################################################
################################################################################
#                 LOAD LIBRARIES
################################################################################
library(tidyverse, quietly=T)
library(plyr, quietly=T)
library(tweenr, quietly=T)
library(animation, quietly=T)
library(rgdal, quietly=T)
library(dplyr, quietly=T)
library(rmapshaper, quietly=T)
library(sp, quietly=T)
library(ggplot2, quietly=T)

################################################################################
#                     DATA
################################################################################
# download and unzip replication data
url <- "https://www.dropbox.com/s/e7zljq0r5vr2k3s/data_confucius_FEB18.csv?dl=1"
download.file(url, destfile="data_confucius_FEB18.csv", method="auto")
#load the dataset
a <- read.csv("data_confucius_FEB18.csv")

#aggregate the dataset, return sum of institutes ('ci')
b1 <- ddply(a, c("city", "start"), summarise, 
	lat=mean(lat), 
	long=mean(long),
	ci=length(city))

# transform data coordinates to Robinson
prj <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
bb <- project(cbind(b1$long, b1$lat), prj)
bb1 <- data.frame(place=b1$city, year=b1$start, long=bb[,1], lat=bb[,2], ci=b1$ci)

# get cumulative institute count
c <- bb1 %>% 
group_by(place) %>% 
dplyr::mutate(value = cumsum(ci)) %>%
na.omit()

# extend the data from min year to max year
year_range <- seq(min(c$year), max(c$year), by=1)
data_expand <- expand.grid(year_range, c$place)
colnames(data_expand) <- c("year", "place")
d <- merge(c, data_expand, by=c("place", "year"), all.y = T) %>% 
ddply(c("place", "year"), 
	summarise, 
	year=max(year), 
	long=max(long),
	lat=max(lat),
	value=max(value)) 

# populate the extended data with info on coordinates and institute count
df <- d %>%
group_by(place) %>% 
complete(year = seq(min(year), max(year), by=1)) %>%
fill(value)%>%
fill(long)%>%
fill(lat)%>%
ungroup()

# split data into frames
x <- split(df, df$year)
# prepare for animation
tm <- tween_states(x, 
	tweenlength= 2, 
	statelength=3, 
	ease=rep('cubic-in-out', 50),
	nframes=50)
tm$year <- round(tm$year, 0) # get rounded years for gif

#download and unrar free world shapefile from Eurostat
url <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2013-01m.shp.zip" # location on the Eurostat website
download.file(url, basename(url), mode="wb") #download Eurostat country shapefiles
unzip("ref-countries-2013-01m.shp.zip") # unzip the boundary data
unzip("CNTR_RG_01M_2013_4326.shp.zip")

#load shapefile
world <- readOGR(getwd(),
        "CNTR_RG_01M_2013_4326", 
         verbose = TRUE, 
         stringsAsFactors = FALSE) %>%
        subset(NAME_ENGL!="Antarctica") # get rid of Antarctica 
        
# transform shp to Robinson projection
wrld <- spTransform(world, CRS("+proj=robin")) 
w <- fortify(wrld)

################################################################################
# ANIMATE MAP
################################################################################
# set limits to make legend fixed
vmin <- min(tm$value, na.rm=T)
vmax <- max(tm$value, na.rm=T)
#0.1 sec for all but the last frame, which is set to 5 secs
times <- c(rep(0.1, max(tm$.frame)-1), 5) 

# plot!
saveGIF({
for(i in 1:max(tm$.frame)) {
  map <-
    tm %>% filter(.frame==i) %>%
    ggplot(aes(x=long, y=lat)) +
geom_map(data = w, map = w,
             aes(map_id = id),
             color = "white", size = .25, fill = "grey80") +
geom_point(aes(size=value), 
	fill="#F30D8C", 
	alpha = .5,
	col="#F30D8C", 
	stroke=.25) +
    scale_size(breaks=c(1, 2, 5, 10, 26), 
    	range = c(1, 8),
    	limits = c(vmin,vmax),
    	name="Institutes")+
guides(size = guide_legend(override.aes = list(alpha = 1),
            direction = "vertical",
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 0,
            nrow = 8,
            byrow = T,
			#labels = labs,
            # also the guide needs to be reversed
            reverse = F,
            label.position = "right"
          )
	) +
    coord_equal() +
  labs(y="", x="",
         title="Confucius Classrooms & Institutes (2004-2015)",
         subtitle=paste0("Year of", " " ,as.character(as.factor(tail(tm %>% filter(.frame==i),1)$year))),
         caption="©2021 Milos Popovic https://milospopovic.net\nData: https://doi.org/10.7910/DVN/CWVIYV/D8QLGY, Harvard Dataverse, V2")+
theme_minimal() +
  theme(legend.position = c(.15, .2),
  	plot.margin=unit(c(-3,-3,-3,-3), "cm"),
    legend.text = element_text(size=18, color="grey20"),
    legend.direction = "horizontal",
    legend.title = element_text(size=20, color="grey20", face="bold"),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "white", size = 0.1),
    panel.grid.minor = element_blank(),
	  plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    legend.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size=30, color="grey20", hjust=.5, vjust=0),
	  plot.caption = element_text(size=18, color="grey20", hjust=.5, vjust=10),
	  plot.subtitle = element_text(size=50, color="#F30D8C", face="bold", hjust=.5),
    strip.text = element_text(size=12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank())

print(paste(i,"out of", max(tm$.frame)))
ani.pause()
print(map)
;}

},movie.name="ci.gif",
interval = times, 
ani.width = 1200, 
ani.height = 900,
other.opts = " -framerate 10  -i image%03d.png -s:v 1200x900 -c:v libx264 -profile:v high -crf 20  -pix_fmt yuv420p")
