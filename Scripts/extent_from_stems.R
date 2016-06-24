
stem_map <- readOGR("./NEONdata/D17-California/SOAP/2013/insitu/veg-structure",
                    "soap_stems")

# first, I have to identify the boundaries of the plots where the stem locations
# are present, since stem locations do not totally match up with centroids

plot(stem_map)
plotid <- stem_map$plotid
names(stem_map)
stem_map$ID <- substr(stem_map$plotid, start = 5, stop = 8)
ID

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

# Create a function to do this
polys <- SpatialPolygons(mapply(function(poly, ID) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)  # take a list and create a matrix
  Polygons(list(Polygon(xy)), ID=ID)
}, split(square, row(square)), ID),proj4string=CRS(as.character("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

stem.polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
stem.polys.df
