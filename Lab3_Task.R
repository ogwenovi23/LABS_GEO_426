
# Installing required R packages for working with spatial data.
# 'sf' is for handling vector data, 'terra' for raster data, and 'spData' for sample datasets.
# 'spDataLarge' contains larger datasets, hosted on a specific repository.

# Questions
# 1.	What is the difference between the sf library and the terra library
# While both sf and terra packages are used for handling spatial data, sf is used 
# for handling vector data while terra is used for handling raster data.

# 2.	There are some common spatial data manipulation functions 
# (GIS shapefile manipulation functions) in the sf library. 
# What are they and what do they do?
# st_read(): This function is used to read spatial data files, such as shapefiles, 
# into R as sf objects.
# st_write(): It writes sf objects to spatial data files, allowing users to save 
# their spatial data in various formats, including shapefile.
# st_polygon() is a function used to create a polygon geometry from a set of 
# coordinates.
# st_union(): It computes the union of geometries in an sf object, combining 
# multiple geometries into a single geometry.
# st_point() function in the sf library in R is used to create a simple feature 
# object representing a single point geometry.
# st_geometry() function in the sf library is used to extract the geometry column 
# from an sf object.
# st_buffer(): This function creates a buffer around geometries in an sf object, 
# expanding or contracting the geometries based on a specified distance.
# st_centroid(): This function calculates the centroids of geometries in an sf 
# object, returning a new sf object with the centroids as points.
# 
# 3.	How would you create a new sf object representing a line or a polygon, 
# given a set of coordinates?
# To create a new sf object representing a line or a polygon, given a set of 
# coordinates, we use the st_linestring() function for lines and the 
# st_polygon() function for polygons.
# 
# 4.	How do you filter spatial data based on a specific attribute, 
# such as area or population?
# To filter spatial data based on a specific attribute, we use the standard R 
# programming subsetting capabilities which include using the dollar sign to 
# specify the attribute and we can set the threshold as required.

# 5.Suppose you have a dataset of weather stations and temperature 
# readings. How would you visually represent areas of high and low temperatures 
# on a map using R?
# To visualize areas of high and low temperatures on a map in R, we can use 
# the sf package for spatial data manipulation and ggplot2 for map creation. 
# After filtering the weather station dataset using dplyr's 
# filter() function to extract pertinent temperature data, 
# we employ geom_sf() from ggplot2 to plot the spatial data. 
# By applying color aesthetics to represent temperature values, 
# we create visually appealing maps showcasing temperature variations 
# across different geographic regions.
# For visualizing high and low temperatures, we can utilize a color gradient, 
# with cooler temperatures depicted in shades of blue and warmer temperatures 
# in shades of red. Each weather station's temperature reading can be plotted 
# on the map using points, where the color of the point corresponds to the 
# temperature value. Stations with lower temperatures will be represented in blue, 
# while those with higher temperatures will be depicted in red.


install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")

# Creating a vector of package names for easier management.
my_packages <- c("sf", "terra", "spData", "sfheaders", 
                 "dplyr", "ggplot2", "deldir", 
                 "classInt", "RColorBrewer", "cartography")
# Checking which packages are not installed and installing them.
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)

# Loading the installed packages into the R session.
library(sf)            # classes and functions for vector data
library(terra)         # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
library(sfheaders)     # helps create spatial data much faster - not necessary for this course but you might need it in the future
library(dplyr)         # several useful and intuitive functions
library(ggplot2)       # useful for visualizations
library(deldir)        # Delaunay triangulation and the Dirichlet or Voronoi tessellation
library(classInt)      # For creating class intervals for map legends
library(RColorBrewer)  # Provides color schemes for maps and graphics
library(cartography)   # Tools for creating thematic maps

#1.	Create your own mixed geometry type with point, line, and polygon geometry and apply a buffer
# Load the sf library
library(sf)

# Creating point geometry
point <- st_point(c(0, 0))

# Creating a line geometry
line <- st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))

# Creating a polygon geometry
polygon <- st_polygon(list(matrix(c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE)))

# Creating a list of geometries
geometries <- list(point, line, polygon)

# Combining point, line, and polygon into a single sf object with mixed geometry types
mixed_geometry <- st_sf(geometry = st_sfc(geometries))

# Plot the mixed geometry
plot(mixed_geometry, main = "Mixed Geometry")

# Apply a buffer to the mixed geometry
buffered_geometry <- st_buffer(mixed_geometry, dist = 0.2)

# Plot the buffered geometry
plot(buffered_geometry, main = "Buffered Geometry")

# 2.	Create two polygons and perform an union that combines both of them

# Defining coordinates for a square polygon
coords_square <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)

# Creating the square polygon
square <- st_polygon(list(coords_square))

# Defining coordinates for a rectangle polygon
coords_rectangle <- matrix(c(1, 0.5, 2, 0.5, 2, 1.5, 1, 1.5, 1, 0.5), ncol = 2, byrow = TRUE)

# Creating the rectangle polygon
rectangle <- st_polygon(list(coords_rectangle))

# Plot the polygons
plot(square, col = "red", main = "Union of Square and Rectangle")
plot(rectangle, col = "blue", add = TRUE)

# Perform a union of the square and rectangle polygons
union_polygon <- st_union(square, rectangle)

# Plot the union polygon
plot(union_polygon, col = "green", add = TRUE)


#3.	Create a mult-layer raster object and produce a jpg output (jpg output is not in the R code – you’ll have to google/Chat GPT how to use it)

# Load the raster library
library(raster)

# Creating a raster layer
raster_layer1 <- raster(matrix(1:100, ncol=10, nrow=10))

# Create another raster layer
raster_layer2 <- raster(matrix(101:200, ncol=10, nrow=10))

# Create a multi-layer raster object
multi_layer_raster <- stack(raster_layer1, raster_layer2)

# Set the file name for the JPG output
jpg_output <- "multi_layer_raster.jpg"

# Plot the multi-layer raster object
plot(multi_layer_raster)

# Save the plot as a JPG file
dev.print(jpg_output, device = jpeg, width = 800, height = 600, units = "px", res = 300)

# 4.The code provides a couple of ways to represent trees as a geographic phenomena. 
#Please write code to represent trees in a different way. Be creative!

library(sf)
library(ggplot2)

# Create an sf object for discrete points (trees)
set.seed(1234)  # Setting a seed for reproducibility of random generation
trees_data <- data.frame(
  id = 1:10,  # Tree identifiers
  x = runif(10, -99.76, -99.74),  # Random x-coordinates for trees within a given range
  y = runif(10, 32.45, 32.46),  # Random y-coordinates for trees within a given range
  canopy_size = runif(10, 5, 15)  # Random canopy size for each tree
)
trees_sf <- st_as_sf(trees_data, coords = c("x", "y"), crs = 4326)  # Convert data frame to spatial features with specified CRS (Coordinate Reference System)

# Update the grid for continuous smooth surface (elevation in feet)
grid <- expand.grid(x = seq(-99.76, -99.74, length.out = 100), 
                    y = seq(32.45, 32.46, length.out = 100))  # Create a grid of points
grid$elevation <- with(grid, (sin(pi * x) + cos(pi * y)) * 2500 + 1500)  # Calculate elevation values for each grid point

# Arid region color palette for elevation
arid_palette <- colorRampPalette(c("saddlebrown", "darkkhaki", "lightyellow"))  # Define a color palette for the elevation map

# Plot the spatial data
ggplot() +
  geom_raster(data = grid, aes(x = x, y = y, fill = elevation), alpha = 0.5) +  # Plot elevation as a raster layer
  geom_sf(data = trees_sf, aes(size = canopy_size), shape = 15, color = "darkgreen", alpha = 0.7) +  # Plot trees as symbols with varying size
  scale_fill_gradientn(colors = arid_palette(100), name = "Elevation (Feet)") +  # Gradient color scale for elevation
  scale_size_continuous(name = "Canopy Size", range = c(2, 8)) +  # Size scale for tree symbols
  labs(title = "Spatial Phenomena: Trees and Smooth Elevation in Arid Region") +  # Title for the plot
  theme_minimal()  # Use a minimal theme for the plot


# 5.	There are several examples of Choropleth maps that use different classification techniques. 
# Please provide a Choropleth map of a variable in the Marriage Dataset that uses a sequential color scheme. 
# Read in shapefile containing Michigan county data
mi_counties <- st_read("C:/Users/Vincent/OneDrive - Michigan State University/Desktop/MSU/Y1S2/GEO 426 Thematic Cartography/LABS/OneDrive_2024-02-08/Lab 3/mi_counties/Counties_(v17a).shp")
# Clean up county names by removing any periods
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)
# Read in CSV file containing marriage rates in Michigan for 2022
marriages <- read.csv("C:/Users/Vincent/OneDrive - Michigan State University/Desktop/MSU/Y1S2/GEO 426 Thematic Cartography/LABS/OneDrive_2024-02-08/Lab 3/MI_Mariage_rates_2022.csv")

# Display the first few rows of each dataset for inspection
head(mi_counties)
head(marriages)

# Join the marriage data with the spatial data of Michigan counties
# and convert certain columns to numeric for analysis
mi_counties <- mi_counties %>% 
  left_join(marriages, by = c('NAME' = 'County')) %>% 
  mutate_at(c("Marriage.Number", "Marriage.Rate", "Divorce.Number", 
              "Divorce.Rate", "Population"), as.numeric)

# Calculate state-wide average marriage rate
state_rate = sum(mi_counties$Marriage.Number) / sum(mi_counties$Population)
# Calculate expected marriage rate for each county based on state average
mi_counties$marriageExp <- state_rate * mi_counties$Population
# Compute relative risk of marriage for each county
mi_counties$relRisk <- ifelse(mi_counties$marriageExp > 0, 100 * (mi_counties$Marriage.Number / mi_counties$marriageExp), 0)

# Plot the spatial data of Michigan counties
plot(mi_counties)

# Visualize the data using ggplot
ggplot(mi_counties) +
  geom_sf(aes(fill = relRisk)) +  # Color counties based on relative risk
  scale_fill_gradient(name = "Relative Risk", low = "lightblue", high = "blue") +  # Sequential color scheme
  theme_minimal()  # Use minimal theme for better visualization

