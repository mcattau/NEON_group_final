
  #Comparing lidar and insitu plot data

# ### ###{r lib-and-settings, message=FALSE}
#library and settings
library(raster)
library(rgdal)
library(ggplot2)
#library(dplyr)
library(plyr)
options(stringsAsFactors = FALSE)

  ###{r set-wd-and-site}
#set working directory and sites of interest
setwd("C:/Users/stellac/Box Sync/Work/neonwksp/data/NEONDI-2016/")

  ###{r load-me}
# set parameters
site_name <- "Soaproot"
site_file_code <- "SOAP"
output_dir_path <- "../outputs/"

#load the raster
SITE_chm <- raster("../NEONDI-2016/NEONdata/D17-California/SOAP/2013/lidar/SOAP_lidarCHM.tif")

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
SITE_plots <- readOGR("./NEONdata/D17-California/SOAP/vector_data",
                      "SOAP_centroids_rev")
ID <- SITE_plots$ID

  ###{r make-spatially-refd-plot-squares}
#
#set the radius for the plots
radius <- 20 #radius in meters

#################################################33
######### Paths diverge here for different plots

#define the plot boundaries based upon the plot radius.
#NOTE: this assumes that plots are oriented North and are not rotated.
#If the plots are rotated, you'd need to do additional math to find
#the corners.
yPlus <- SITE_plots$northing+radius
xPlus <- SITE_plots$easting+radius
yOrigin <- SITE_plots$northing
xOrigin <- SITE_plots$easting

#yMinus <- SITE_plots$northing-radius
#xMinus <- SITE_plots$easting-radius
#square= cbind(xMinus, yPlus,xPlus, yPlus, xPlus,  yMinus,xMinus,yMinus,xMinus,yPlus,xMinus,yPlus)


#replace this with k. brauzina's create plot extents. now in git.
square4 = cbind(xOrigin,yPlus,#N
               xPlus,yPlus,#NE
               xPlus,yOrigin,#E
               xOrigin,yOrigin)#Origin/Center

split(square, row(square))

#look up spatial reference
SITE_plots@proj4string

##create spatial polygons
polys <- SpatialPolygons(mapply(function(poly, ID)  {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=ID)
}, split(square4, row(square4)), ID),proj4string=CRS(as.character("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

X11()
plot(polys, main = "square4 test")

  ###{r create-spdf}
#Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

plot(SITE_chm)
plot(polys.df, add=T)

  ###{r extract-by-polygon}
# Insitu sampling took place within 40m x 40m square plots, so we use a 20m radius.
# Note that below will return a dataframe containing the max height
# calculated from all pixels in the buffer for each plot
SITE_height_mean <- extract(SITE_chm,
                            polys.df,
                            fun=mean ,
                            na.rm = TRUE,
                            sp=TRUE, # spatial object with max values, not just vals.
                            stringsAsFactors=FALSE)

SITE_height_mean # this is a spatial points data frame

ht_mean <- data.frame(SITE_height_mean$id, SITE_height_mean$SOAP_lidarCHM )

SITE_height_max <- extract(SITE_chm,
                           polys.df,
                           fun=max ,
                           na.rm = TRUE,
                           sp=TRUE, # spatial object with max values, not just vals.
                           stringsAsFactors=FALSE)

SITE_height_max$SOAP_lidarCHM

ht_max <- data.frame(SITE_height_max$id, SITE_height_max$SOAP_lidarCHM )

ht_mean
ht_max

  ###{r import-in-situ}
# import the centroid data and the vegetation structure data
SITE_insitu <- read.csv("C:/Users/stellac/Box Sync/Work/neonwksp/data/NEONDI-2016/NEONdata/D17-California/SOAP/2013/insitu/veg-structure/D17_2013_SOAP_vegStr.csv")
#replace plotid with ID so it matches what we want to join by
SITE_insitu$ID <- substr(SITE_insitu$plotid,start=5,stop=10)
unique(SITE_insitu$ID )
#18 plots in the insitu data

SITE_insitu$ID
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
summary(SITE_height_mean@data$insituHtValue)
### these are returning lots of NA values (17/50 are values)

  ###{r describe-me}
ggplot(SITE_height_max@data, aes(x=SOAP_lidarCHM, y = insituHtValue)) +
  geom_point() +
  theme_bw() +
  ylab("Maximum measured height") +
  xlab("Maximum LiDAR pixel")+
  geom_abline(intercept = 0, slope=1) +
  ggtitle("Lidar Height Compared to InSitu Measured Height")

  ###{r plot-gg-regression}
##plot with regression fit
p <- ggplot(SITE_height_max@data, aes(x = SOAP_lidarCHM, y = insituHtValue)) +
  geom_point() +
  ylab("Maximum Measured Height") +
  xlab("Maximum LiDAR Height") +
  geom_abline(intercept = 0, slope=1) +
  geom_smooth(method = lm)

p + theme(panel.background = element_rect(colour = "grey")) +
  ggtitle("SOAP LiDAR CHM Derived vs Measured Tree Height") +
  theme(plot.title=element_text(family="sans", face="bold", size=20, vjust=1.9)) +
  theme(axis.title.y = element_text(family="sans", face="bold", size=14, angle=90,   hjust=0.54, vjust=1)) +
  theme(axis.title.x = element_text(family="sans", face="bold", size=14, angle=00, hjust=0.54, vjust=-.2))


##Possible issues
The plots are not lined up with the actual tree locations! (Personal Communication, ArcGIS)