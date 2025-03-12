# Make Figure 1 map
# Alec Chiono, alec.chiono@colorado
# Created 2024-12-06
librarian::shelf(tidyverse, raster, sf, prettymapr, mapdata)

# Create DEM directory if necessary
if(!file.exists("output")) dir.create("output")
if (!file.exists("output/dems")) dir.create("output/dems")

# Get DEM urls
dem_urls <- read.csv("data/dem_urls.csv", header=FALSE)[,26]

# Download DEM files if necessary
for(i in 1:length(dem_urls)) {
  tmp_file.name <- paste0("output/dems/", tail(str_split(dem_urls[i], "/")[[1]], n=1))
  if(!file.exists(tmp_file.name)) {
    options(timeout=max(1000, getOption("timeout")))
    download.file(dem_urls[i], destfile=tmp_file.name)
  }
}

# Get DEM file.names
dem_files <- list.files(path="output/dems", pattern="\\.tif$", full.names=TRUE)

# Read in DEM files, crop to plotting extent, then merge together
for(i in 1:length(dem_files)) {
  if(i==1) {
    dem <- crop(raster(dem_files[[i]]), extent(-123.1, -122, 37.25, 39))
  } else {
    dem <- merge(dem, crop(raster(dem_files[[i]]), extent(-123.1, -122, 37.25, 39)))
  }
}

# Read in site information
site_info <- read.csv("data/site_info.csv") %>% #read site info
  st_as_sf(
    coords=c("longitude", "latitude"),
    crs=crs(dem)) %>% 
  as("Spatial")

# Read in water body shape files
water_bodies <- st_read("data/region_water_area_-8493199990115632496/", "region_water_area") %>% 
  filter(!is.na(wname)) #remove bodies of water that are not named

# Estimate ocean area that isn't already covered by water_bodies shapefile (FYI takes a while)
ocean_est <- overlay(dem, terrain(dem), fun=function(x, y) ifelse(x==0 & y==0, 1, 0)) #find cells with elevation and slope both 0

# Combine ocean and dem to create raster for plotting
plot_dem <- overlay(dem, ocean_est, fun=function(x,y) ifelse(y==1, NA, x))

# Crop Farallon Islands to add to plot separately since water bodies covers it
farallons <- crop(plot_dem, extent(-123.1, -122.9, 37.65, 37.75))

# Define colors
dem_colors <- colorRampPalette(c("darkgreen" , "wheat", "chocolate4"), bias=2.7) #create color scale function to color topography
water_color <- "skyblue4"
ecotype_color <- ifelse(site_info$ecotype=="Semelparous", "grey75", "grey25")
ecotype_hc <- ifelse(site_info$ecotype=="Semelparous", "black", "grey25")

# Figure out plot ratio and dimensions
plot_ratio <- dim(dem)[1]/dim(dem)[2]*1.1
plot_width <- 5

# Get info for inset map
dem_2corners <- extent(plot_dem) %>% as.matrix() %>% t() %>% as.data.frame()
dem_extent <- expand_grid(x=dem_2corners$x, y=dem_2corners$y) %>% .[c(1,2,4,3),]
inset_poly <- map_data("state") %>% filter(region=="california")

# Create photo panels
plot_jpeg = function(path, add=FALSE)
{
  require('jpeg')
  jpg = readJPEG(path, native=T) # read the file
  res = dim(jpg)[2:1] # get the resolution, [x, y]
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[1],res[2])
}

jpeg("photos/plants.jpg", height=2000, width=1000)
par(mfrow=c(2,1), mar=c(0,0,0,0))
plot_jpeg("photos/S3.jpg")
text(75, 1000, "b", col="white", family="Times", cex=8)
plot_jpeg("photos/I1.jpg")
text(50, 730, "c", col="white", family="Times", cex=9)
dev.off()

# Plot
png("figures/figureS1.png", width=plot_width*plot_ratio, height=plot_width*plot_ratio, units="in", res=1500)
par(mfrow=(c(1,2)))
par(bty="n", bg=water_color, mar=c(0,0,0,0), oma=c(0,0,0,0), family="Times")
image(plot_dem, col=dem_colors(100), axes=FALSE) #plot dem
plot(water_bodies, col=water_color, border=water_color, add=TRUE) #plot water bodies
plot(farallons, col=dem_colors(100)[1:10], legend=FALSE, add=TRUE) #add Farallon Islands
points(site_info, pch=21, cex=1, bg=ecotype_color, col=ecotype_hc) #plot site points
text(-123.05, 38.95, "a", col="black", family="Times", cex=2.75)
text(site_info, labels=site_info$site_code, 
     col=ecotype_color, halo=TRUE, hc=ecotype_hc,
     pos=c(2, 4, 3, 3, 1, 3), cex=3, font=2)
addscalebar(plotunit="latlon", unit="metric")
par(usr=c(min(inset_poly$long)-1.9, min(inset_poly$long)+40.1, min(inset_poly$lat)-3, min(inset_poly$lat)+52))
rect(xleft=min(inset_poly$long)-.5, xright=max(inset_poly$long)+.5, ybottom=min(inset_poly$lat)-.5, ytop=max(inset_poly$lat)+.5, col="white", border="black")
polygon(inset_poly$long, inset_poly$lat, col="gray", border="gray")
polygon(dem_extent$x, dem_extent$y, col=NA, border="black")
plot_jpeg("photos/plants.jpg")
dev.off()
