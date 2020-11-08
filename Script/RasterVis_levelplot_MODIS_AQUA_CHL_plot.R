
#Chlorophyll_a concentration plot
#Tuan-Anh Bui
#Email: tuananh.bui@imbrsea.eu
#Deme 2019-05-23


# Install and load prerequisite libraries ---------------------------------

install.packages(c("raster","ncdf4","rasterVis","zoo","tidyverse","rgdal"))

{ library(raster)
library(ncdf4)
library(rasterVis)
library(zoo)
library(tidyverse)
library(rgdal)
}


# Check locale to the US to have international text display -------------

Sys.getlocale() #If not English, then change to English
Sys.setlocale("LC_ALL","English") 


# Load dataset ------------------------------------------------------------

##Load netCDF files 
#Create a list of file and indicate its length
(f <- list.files(path = "C:/Users/TABU/Desktop/wget/MODIS_Aqua_Monthly_CHL_chlor_a_01.2006_04.2019/", pattern="*.L3m_MO_CHL_chlor_a_4km.nc",full.names=T))
#Lenght of file list
(lf<-length(f))

#Explore the netCDF file
nc_open(f[1])

#Indicate variable
var <- "chlor_a"

##Convert netCDF files to StackRaster format
data_stack <- stack(f, varname = var) #Indicate variable as chlor_a
data_stack

##Load shapefiles Add GSR and CCZ regions 
#Identify the directory of the shapefile
SHP_dir <- "C:/Users/TABU/Desktop/Tuan-Anh Bui/2_Deep sea/R_analysis/Data/GSR_coordinates/shapefiles"

#Identify name of shapefile files
GSR_name <- "GSR" #Can use pattern to select different files
CCZ_name <- "CCZ"

#Add GSR Shapefile
GSR <- readOGR(SHP_dir,GSR_name) 
#Add CCZ Shapefile
CCZ <- readOGR(SHP_dir,CCZ_name) 

##Crop area of interest
#Create extent for area of interest - GSR region
GSR #Load GSR to see the extent of the area
AOI <- extent(-129,-121,11,17) 

# Create data frame with information of files in area of interest ---------
#Create an empty data frame 
df <- tibble(year = integer(),
             month = character(),
             value_min = double(),
             value_max = double(),
             unit = character(),
             var = character ()
) 

#Set up loop function - use seq_along(f)
for (i in 1:lf) {
  # progress indicator
  print(paste("Processing file",i,"from",length(f),sep=" "))
  
  # open netCDF file
  data <- nc_open(f[i])
  
  # Extract information from netCDF file
  # Extract date
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year <- format(datemean, "%Y")
  month <- format(datemean, "%B")
  #Extract unit
  unit <- ncatt_get(data,var,"units")$value
  
  #Extract value from raster file (in arae of interest)
  value_min <- min(raster::extract(data_stack[[i]], AOI), na.rm = T)
  value_max <- max(raster::extract(data_stack[[i]], AOI), na.rm = T)
  
  #Creat temporary data frame
  temp_df <- tibble(as.integer(year),month,value_min, value_max, unit, var) 
  names(temp_df) <- c("year","month","value_min","value_max","unit","var")
  df <- rbind(df, temp_df)
  rm(data, value_min, value_max, dateini, dateend, datemean, year, month)
}


# Prepare raster data to plot ---------------------------------------------

##Add time index - from 2006-01 to 2019-04
#Create a vector of time 
#See start date of dataset
ncatt_get(nc_open(f[1]),0,"time_coverage_start")$value 

#Set the middle of month as time index of monthly dataset
timeIndex <- seq(as.Date("2006-01-15"), by = "month", length = length(f))
length(timeIndex) == lf #TRUE?
#Lenght of timeIndex must be the same as number of files

#Crop RasterStack by arae of interest
data_AOI <- crop(data_stack, AOI)

#Set the time as z dimension of RasterStack
data_AOI_time <- setZ(data_AOI, timeIndex)

#Change names of data_AOI_time layers corresponding to time period
#The formate of name is Month_Year
names(data_AOI_time) <- paste0(df$month,"_", df$year)

crs(data_AOI_time); GSR #Check to see if crs (Coordinate Reference System) of the 2 files are not the same
#In this case they are the same


# Plot data ---------------------------------------------------------------
##Set color ramp
RLcolor <- colorRampPalette(c("royalblue","skyblue","green","yellow","red"),bias=1,space="Lab",interpolate="linear")

#Indicate max value of chlorophyll a concentration to set range of legend
value_max <- max(df$value_max, na.rm = T)
#Explore the distribution of value_max 
ggplot(df) + geom_freqpoly(aes(value_max), binwidth = 0.1) 
#Majority of max values are under 5, there are a few values larger than 30

#Take a closer look to decide the maximum range of color legend
ggplot(df) + geom_freqpoly(aes(value_max), binwidth = 0.1) + coord_cartesian(xlim = c(0,5))
#Most of values are under 1

#We will set the maximum range of legend to 1 for better visualization
#A better way is to set the legend to logarithm scale but the transformation is no available at the momment of making the script
legend_range <- c(seq(0,1,by=0.01)) #Set division of the legend as 100 
n <- length(legend_range)

##Plot 
levelplot(data_AOI_time, 
          layout = c(1,1), #Create a 1x1 layout for the data
          layers=1:1, #Indicate layers to display 
          main = paste0("Monthly chlorophyll concentration (mg/m³) at GSR areas in ",
                        "\n NASA AQUA MODIS images"),
          margin = F,
          col.regions=RLcolor(n), at = legend_range) +
  latticeExtra::layer(sp.polygons(GSR)) 
#latticeExtra is used to avoid mistaken for the same function of tidyverse


#Export plots
trellis.device(png, #Indicate output format,
               file=paste0("C:/Users/TABU/Desktop/Tuan-Anh Bui/2_Deep sea/R_analysis/Figures/MODIS AQUA CHL_%01d.png"),
               #Indicate file location,
               res=300, width=3300, height=3500)
levelplot(data_AOI_time, 
          layout = c(3,4), #Create a 3x4 layout for the data
          main = paste0("Monthly chlorophyll concentration (mg/m³) at GSR areas in ",
                        "\n NASA AQUA MODIS images"),
          margin = F,
          col.regions=RLcolor(n), at = legend_range) +
  latticeExtra::layer(sp.polygons(GSR)) 
#latticeExtra is used to avoid mistaken for the same function of tidyverse
dev.off()

#Hovmoller diagram - could be considered

##Animation
#Animate on R
animate(data_AOI_time, main = names, pause = 0.5,  col=RLcolor(10))






#Add name for each layer of RasterStack corresponding to netCDF files

