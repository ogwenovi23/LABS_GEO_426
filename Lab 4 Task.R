# Questions
# 1.	When would you want to change the scale of your data?
#   
#   Altering the scale of data is a fundamental process to better match the 
#   needs of the intended audience. It allows for the presentation of complex 
#   information clearly and understandably. Scaling down large-scale data, 
#   for example, enables the representation of vast geographic areas on 
#   smaller maps, making them easier to read and interpret. 
#   Conversely, scaling up small-scale data allows for a more detailed examination 
#   of a smaller region, enhancing precision and detail. 
#   Adjusting the scale of data is a crucial part of creating maps that effectively 
#   communicate information to different audiences with varying levels of detail 
#   requirements.
# 
# 2.	What are some pros and cons of changing your raster resolution to have less resolution?
#   
#   Advantageously, reducing raster resolution can reduce the file size and 
#   computational demands, allowing for faster processing and easier sharing of data. 
#   This can be particularly advantageous when working with large datasets or 
#   limited computational resources. 
#   Furthermore, lower resolution can sometimes improve the visual appearance of 
#   the data, smoothing out small variations and making the overall patterns more 
#   apparent. However, reducing resolution can also lead to loss of information, 
#   resulting in a less accurate representation of the underlying data. 
#   This can be problematic, particularly in scientific or engineering 
#   applications where precision is crucial. It can also limit the range of 
#   analyses that can be performed, as certain types of analysis require 
#   high-resolution data to provide accurate results.
# 
# 3.	When would you want to generalize your polygon data (think countries, states, counties, etc)
# 
# Generalizing polygon data, such as countries, states, or counties, is typically
# necessary to simplify complex shapes and reduce the level of detail for specific
# mapping purposes. This simplification is essential for creating visually clear 
# and uncluttered maps, particularly when representing large geographic areas or
# when aiming for a more generalized overview. Generalization helps in emphasizing
# broader patterns and relationships within the data while minimizing visual 
# distractions caused by excessive detail. It also aids in optimizing map 
# performance and readability, making it easier for users to interpret and 
# comprehend the information presented on the map.
# 
# 4.	How is simplifying line data different from smoothing the line data?
# 
# Simplifying line data involves reducing the number of points that make up a 
# line, without significantly altering its overall shape, while smoothing line 
# data involves removing sharp corners or jagged edges by applying mathematical 
# algorithms that smooth the line's curvature. 
# Simplification maintains the essential shape of the line but reduces its 
# complexity, whereas smoothing focuses on altering the line's appearance to 
# make it appear smoother or more continuous. Simplification is more focused 
# on data reduction and optimization for rendering, while smoothing is more 
# concerned with improving visual aesthetics.
# 
# 5.	When we are aggregating point to polygons in what situation do you want to use a concave approach? A convex approach?
# When aggregating points to polygons, a concave approach might be preferred when
# the points being aggregated are scattered throughout the polygon and the goal 
# is to create smaller, more distinct aggregations within the polygon boundaries.
# Additionally, a concave approach can be advantageous when trying to create 
# polygons that follow natural boundaries, such as rivers or coastlines.
# Conversely, a convex approach might be more suitable when dealing with points 
# that are more uniformly distributed within the polygon, or when the goal is to 
# create larger, more inclusive aggregations. 
# This could be the case in land-use planning where the goal is to create 
# generalized areas for different types of development or zoning. 
# A convex approach may also be preferred in cases where the resulting 
# polygon needs to be easily interpreted and compared to other polygons, 
# as convex polygons are more regular and predictable in shape. 
# Otherwise, neither method is universally superior and the best choice will 
# depend on the specific objectives and characteristics of the dataset.


#1.	Take the world map object and subset to Germany and produce a jpeg image. 
#Submit your code and the Germany jpeg image. 

# 1. PACKAGES

libs <- c(
  "terra",
  "giscoR",
  "sf",
  "tidyverse",
  "ggtern",
  "elevatr",
  "png",
  "jpeg",
  "rayshader",
  "magick"
)

installed_libraries <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libraries == F)){
  install.packages(
    libs[!installed_libraries]
  )
}

invisible(
  lapply(
    libs, library, character.only = T
  )
)

# 2. COUNTRY BORDERS

country_sf <- giscoR::gisco_get_countries(
  country = "DE",
  resolution = "1"
)

plot(sf::st_geometry(country_sf))

png("Germany-borders.png")
plot(sf::st_geometry(country_sf))
dev.off()


################################################################################
################################################################################
########################### Changing Resolution ################################
################################################################################
################################################################################


# Load required library
library(raster)
library(sf)
library(raster)
library(ggplot2)
library(maps)
library(maptools)
library(mapdata)
library(ggspatial)
library(concaveman)
library(smoothr)

# Read the image file as a raster object
#mt_raster <- raster("C:/Users/Vincent/OneDrive - Michigan State University/Desktop/MSU/Y1S2/GEO 426 Thematic Cartography/LABS/Lab 4/Lab 4/mountain.jpg")

# Plot the RGB image
#plot(mt_raster, main = "High-Resolution RGB Image")




# 2.	Change the resolution of the volcano dataset to a factor of 30 
# and find the ‘max’ cell stat. 
data(volcano)
volcano_raster <- raster(volcano)
plot(volcano_raster, main = "High-Resolution Raster")

# Raster Resolution of factor 30
# Aggregate the data to lower the resolution
volcano_fact_30 <- aggregate(volcano_raster, fact = 30)
plot(volcano_fact_30, main = "Resolution Raster (Factor = 30)")

# Find 'max' cell statistic for volcano_fact_30 dataset
max_value <- cellStats(volcano_fact_30, stat = "max")
max_value

#3.	We did some smoothing operations. Please smooth the North Carolina 
#dataset using the ‘ksmooth’ and ‘spline’ approaches and plot them to the
#screen on a 2x2 grid.
# Original Vector Data
nc <- st_read(system.file("shape/nc.shp", package="sf"))
plot(st_geometry(nc), main="Original North Carolina Shapefile")

#Ksmooth and spline smoothing
smoothed_nc_ksmooth <- smooth(nc, method = 'ksmooth')
smoothed_nc_spline <- smooth(nc, method = 'spline')
par(mfrow=c(2,2))
plot(st_geometry(smoothed_nc_ksmooth), main="KSmoothed")
plot(st_geometry(smoothed_nc_ksmooth), main="Spline_Smoothed")

#4.	Plot your own convex and concave polygon using your own point dataset 
#(you might have to browse the internet for a set of spatial points data).

library(sf)
library(concaveman)

# Define imaginary spatial points
imaginary_points <- matrix(runif(80, min = -5, max = 40), ncol = 2)
colnames(imaginary_points) <- c("x", "y")
imaginary_points_sf <- st_as_sf(data.frame(imaginary_points), coords = c("x", "y"), crs = 4326)

# Create convex and concave polygons from the points
convex_hull_polygon <- st_convex_hull(st_combine(imaginary_points_sf))
concave_hull_polygon <- st_sf(geometry = concaveman(imaginary_points_sf))

# Set up layout
par(mfrow=c(2,2))

# Plot the results
plot(st_geometry(imaginary_points_sf), col = 'blue', pch = 20, cex = 0.5, main = "Imaginary Locations")
plot(st_geometry(convex_hull_polygon), col = 'red', border = 'black', lwd = 2, main = "Convex Hull")
plot(st_geometry(concave_hull_polygon), col = 'green', border = 'black', lwd = 2, main = "Concave Hull")

# Create a blank plot for the legend
plot.new()

# Add a legend
legend("topright", legend = c("Points", "Convex Hull", "Concave Hull"),
       col = c("blue", "red", "green"), pch = c(20, NA, NA), lty = c(NA, 1, 1), cex=0.8)

