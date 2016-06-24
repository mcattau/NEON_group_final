
#Comparing lidar and insitu plot data

# ### ###{r lib-and-settings, message=FALSE}
#library and settings
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
#library(plyr)
options(stringsAsFactors = FALSE)


###{r set-wd-and-site}
#set working directory and sites of interest
setwd("C:/Users/stellac/Box Sync/Work/neonwksp/data/NEONDI-2016/")

###{r load-me}
# set parameters
site_name <- "SJERroot"
site_file_code <- "SJER"
output_dir_path <- "../outputs/"

#load the raster
SITE_chm <- raster("../NEONDI-2016/NEONdata/D17-California/SJER/2013/lidar/SJER_lidarCHM.tif")

###{r inspect-and-set-values-to-na, echo=FALSE}
#
#set values to na if 0
SITE_chm[SITE_chm==0] <- NA
density(SITE_chm)

###{r read-plot-locations}
# bring in the plot locations
# import plot SITE_plots
# rgdal readOGR is a vector reader; these are points
# note that path has no extension
# these are revised as of Thursday morning

####
stem_map <- readOGR("./NEONdata/D17-California/SOAP/2013/insitu/veg-structure",
                    "soap_stems")

# first, I have to identify the boundaries of the plots where the stem locations
# are present, since stem locations do not totally match up with centroids

plotid <- stem_map$plotid
names(stem_map)
stem_map$ID <- substr(stem_map$plotid, start = 5, stop = 8)
stem_map$ID

# i'm extracting the northing and easting max and min and
# adding or subtracting 5 as a buffer
stem_map.extent <- stem_map@data %>%
  dplyr::group_by(plotid) %>%
  dplyr::summarise(northing.max = max(northing) + 5,
                   northing.min = min(northing) - 5,
                   easting.max = max(easting) + 5,
                   easting.min = min(easting) - 5)
stem_map.extent <- data.frame(stem_map.extent)

yPlus <- stem_map.extent$northing.max
yMinus <- stem_map.extent$northing.min
xPlus <- stem_map.extent$easting.max
xMinus <- stem_map.extent$easting.min
#names(stem_map.extent) <- c("plotid","yPlus","yMinus","xPlus","xMinus")
# copying code from group uncertainty

square <- cbind(xMinus, yPlus,
                xPlus, yPlus,
                xPlus, yMinus,
                xMinus, yMinus,
                xMinus, yPlus,
                xMinus, yPlus)

ID <- stem_map.extent$plotid

polys <- SpatialPolygons(mapply(function(poly, ID) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)  # take a list and create a matrix
  Polygons(list(Polygon(xy)), ID=ID)
}, split(square, row(square)), ID),proj4string=CRS(as.character("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

stem.polys.spdf <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
stem.polys.spdf
X11()
plot(stem.polys.spdf, main = "stem.polys.spdf")

###{r extract-by-polygon}
# Insitu sampling took place within 40m x 40m square plots, so we use a 20m radius.
# Note that below will return a dataframe containing the max height
# calculated from all pixels in the buffer for each plot
SITE_height_mean <- extract(SITE_chm,
                            polys,
                            fun=mean ,
                            na.rm = TRUE,
                            sp=TRUE, # spatial object with max values, not just vals.
                            stringsAsFactors=FALSE)

SITE_height_mean # this is a spatial points data frame

ht_mean <- data.frame(SITE_height_mean$id, SITE_height_mean$SJER_lidarCHM )

SITE_height_max <- extract(SITE_chm,
                           polys,
                           fun=max ,
                           na.rm = TRUE,
                           sp=TRUE, # spatial object with max values, not just vals.
                           stringsAsFactors=FALSE)

SITE_height_max$SJER_lidarCHM

ht_max <- data.frame(SITE_height_max$id, SITE_height_max$SJER_lidarCHM )

ht_mean
ht_max

###{r import-in-situ}
# import the centroid data and the vegetation structure data
#if this breaks check veg-structure vs. veg_structure
SITE_insitu <- read.csv("NEONdata/D17-California/SJER/2013/insitu/veg_structure/D17_2013_SJER_vegStr.csv")
SITE_insitu$plotid
#replace plotid with ID so it matches what we want to join by
SITE_insitu$ID <- substr(SITE_insitu$plotid,start=5,stop=10)
ID_insitu <-unique(SITE_insitu$ID )
SITE_insitu$ID
ID_insitu; ID

#18 plots in the insitu data
insitu_maxStemHeight <- ddply(SITE_insitu,.(ID),summarize,max = max(stemheight,na.rm=T))
insitu_meanStemHeight <- ddply(SITE_insitu,.(ID),summarize,max = mean(stemheight,na.rm=T))

# let's create better, self documenting column headers
names(insitu_maxStemHeight) <- c("plotid","insituHtValue")
head(insitu_maxStemHeight)

# let's create better, self documenting column headers
names(insitu_meanStemHeight) <- c("plotid","insituHtValue")
head(insitu_meanStemHeight)

# merge to create a new spatial df; adds insituHtValue (max) to SITE_Ht
SITE_height_max@data <- data.frame(SITE_height_max@data,
          insitu_maxStemHeight[match(SITE_height_max@data[,"id"],
          insitu_maxStemHeight$plotid),])

SITE_height_max@data$insituHtValue

SITE_height_mean@data <- data.frame(SITE_height_mean@data,
             insitu_meanStemHeight[match(SITE_height_mean@data[,"id"],
            insitu_meanStemHeight$plotid),])

SITE_height_mean@data$insituHtValue

###{r describe-me}
ggplot(SITE_height_max@data, aes(x=SJER_lidarCHM, y = insituHtValue)) +
  geom_point() +
  theme_bw() +
  ylab("Maximum measured height") +
  xlab("Maximum LiDAR pixel")+
  geom_abline(intercept = 0, slope=1) +
  ggtitle("Lidar Height Compared to InSitu Measured Height")

###{r plot-gg-regression}
##plot with regression fit
p <- ggplot(SITE_height_max@data, aes(x = SJER_lidarCHM, y = insituHtValue)) +
  geom_point() +
  ylab("Maximum Measured Height") +
  xlab("Maximum LiDAR Height") +
  geom_abline(intercept = 0, slope=1) +
  geom_smooth(method = lm)

p + theme(panel.background = element_rect(colour = "grey")) +
  ggtitle("SJER LiDAR CHM Derived vs Measured Tree Height") +
  theme(plot.title=element_text(family="sans", face="bold", size=20, vjust=1.9)) +
  theme(axis.title.y = element_text(family="sans", face="bold", size=14, angle=90,   hjust=0.54, vjust=1)) +
  theme(axis.title.x = element_text(family="sans", face="bold", size=14, angle=00, hjust=0.54, vjust=-.2))
