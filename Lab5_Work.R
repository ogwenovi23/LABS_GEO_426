
#Name: Vincent Ogweno
#Lab 5

library(sf)
library(dplyr)
library(RColorBrewer)
library(mapsf) #https://github.com/riatelab/mapsf/tree/69d76e3d3a49b12c02b16df458b0f04dac2b21cd
library(dplyr)

################################################################################
############################### Load Data ################################
################################################################################

mi_counties <- st_read("C:/Users/Vincent/OneDrive - Michigan State University/Desktop/MSU/Y1S2/GEO 426 Thematic Cartography/LABS/Lab 5-20240220T223418Z-001/Lab 5/mi_counties/Counties_(v17a).shp")
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)
marriages <- read.csv("C:/Users/Vincent/OneDrive - Michigan State University/Desktop/MSU/Y1S2/GEO 426 Thematic Cartography/LABS/Lab 5-20240220T223418Z-001/Lab 5/MI_Mariage_rates_2022.csv")

mi_counties <- mi_counties %>% 
  left_join(marriages, by = c('NAME' = 'County')) %>% 
  mutate_at(c("Marriage.Number", "Marriage.Rate", "Divorce.Number", 
              "Divorce.Rate", "Population"), as.numeric)

state_rate = sum(mi_counties$Marriage.Number) / sum(mi_counties$Population)
mi_counties$marriageExp <- state_rate * mi_counties$Population
mi_counties$relRisk <- ifelse(mi_counties$marriageExp > 0, 100 * (mi_counties$Marriage.Number / mi_counties$marriageExp), 0)

#Make some ordinal data
breaks <- c(-Inf, 40, 600, 1500, Inf)
labels <- c("Very Low Marriages", 
            "Moderate Marriages", "High Marriages", 
            "Very High Marriages")
mi_counties$OrdinalMarriages<- cut(mi_counties$Marriage.Number, 
                                   breaks = breaks, 
                                   labels = labels, 
                                   include.lowest = TRUE, 
                                   ordered_result = TRUE)
table(mi_counties$OrdinalMarriages)


#Let's make some nominal data
mi_counties$CountyLetter<-substr(mi_counties$NAME, 1, 1)
table(mi_counties$CountyLetter)


#R has its own library of custom colors should you choose to ever take that route
head(colors())
length(colors()) #657 colors to choose from



#1

#What is going on in this code? 
my_color_names<-c("#f1eef6", "#d7b5d8", "#df65b0", "#ce1256")
my_colors <- as.hexmode( c(256^(2:0) %*% col2rgb(my_color_names)) )
my_colors<-paste0("#", my_colors)
plot(1:length(my_color_names), rep(1, length(my_color_names)), 
     bg = my_colors, pch = 22, cex = 4)

#we can also create a nice table of color names and hexcodes
my_color_data<-data.frame('Color Names' = my_color_names,
                          'Hex Codes' = my_colors)


# Update the palette in the mf_map function
mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       pal = my_colors,
       nbreaks = length(my_colors))



#2

#Let's look at ordinal marriages
ordinalBrewer <- brewer.pal(n = 4, name = "Set3")
mf_map(mi_counties, 
       var = "OrdinalMarriages",
       type = "typo",
       pal = ordinalBrewer) 


#3

mi_counties <- st_read("C:/Users/Vincent/OneDrive - Michigan State University/Desktop/MSU/Y1S2/GEO 426 Thematic Cartography/LABS/Lab 5-20240220T223418Z-001/Lab 5/mi_counties/Counties_(v17a).shp")

#We can use the print statement to identify what, if any, projection the sf object has
print(mi_counties$geometry) #This should show us several pieces of important geogrpahic infomration
#EPSG Approach
mi_counties<-st_transform(mi_counties, 3082) #These four digits represent EPSG of 3857 which is a Web Mercator projection
plot(st_geometry(mi_counties))


#4

world <- st_read("C:/Users/Vincent/OneDrive - Michigan State University/Desktop/MSU/Y1S2/GEO 426 Thematic Cartography/LABS/Lab 5-20240220T223418Z-001/Lab 5/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
plot(st_geometry(world))

# Filter for Lithuania
lithuania <- world %>% filter(NAME == "Lithuania")

# Plot Lithuania# Plot Lithuania
plot(st_geometry(lithuania))



# Project the data into WGS 1984 (EPSG:4326)
lithuania_wgs1984 <- st_transform(lithuania, crs = "+proj=longlat +datum=WGS84")

# Project the data into an equal-area projection
lithuania_equal_area <- st_transform(lithuania, crs = "+proj=laea +lat_0=52 +lon_0=24 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

par(mfrow=c(1,2))
# Plot Lithuania in WGS 1984
plot(st_geometry(lithuania_wgs1984),main="WGS1984 Projection")
# Plot Lithuania in an equal-area projection
plot(st_geometry(lithuania_equal_area), main="Equal Area Projection")

#5

mi_counties <- st_read("C:/Users/Vincent/OneDrive - Michigan State University/Desktop/MSU/Y1S2/GEO 426 Thematic Cartography/LABS/Lab 5-20240220T223418Z-001/Lab 5/mi_counties/Counties_(v17a).shp")

#We can use the print statement to identify what, if any, projection the sf object has
print(mi_counties$geometry) #This should show us several pieces of important geogrpahic infomration
#EPSG Approach
mi_counties<-st_transform(mi_counties, crs = "+proj=longlat +datum=WGS84") 

par(mfrow=c(1,1))
plot(st_geometry(mi_counties))

mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)
marriages <- read.csv("C:/Users/Vincent/OneDrive - Michigan State University/Desktop/MSU/Y1S2/GEO 426 Thematic Cartography/LABS/Lab 5-20240220T223418Z-001/Lab 5/MI_Mariage_rates_2022.csv")

mi_counties <- mi_counties %>% 
  left_join(marriages, by = c('NAME' = 'County')) %>% 
  mutate_at(c("Marriage.Number", "Marriage.Rate", "Divorce.Number", 
              "Divorce.Rate", "Population"), as.numeric)

state_rate = sum(mi_counties$Marriage.Number) / sum(mi_counties$Population)
mi_counties$marriageExp <- state_rate * mi_counties$Population
mi_counties$relRisk <- ifelse(mi_counties$marriageExp > 0, 100 * (mi_counties$Marriage.Number / mi_counties$marriageExp), 0)


#Make some ordinal data
breaks <- c(50,100, 200, 400, 600, 800)
labels <- c("Very Low Marriages", "Low Marriages", 
            "Moderate Marriages", "High Marriages", 
            "Very High Marriages")
mi_counties$OrdinalMarriages<- cut(mi_counties$Marriage.Number, 
                                   breaks = breaks, 
                                   labels = labels, 
                                   include.lowest = TRUE, 
                                   ordered_result = TRUE)
table(mi_counties$OrdinalMarriages)


mp7 <- brewer.pal(7, "RdBu")
#mp7<-rev(mp7) 
mf_map(mi_counties, 
       var = "relRisk",
       type = "choro",
       breaks = breaks,
       pal = mp7) 
