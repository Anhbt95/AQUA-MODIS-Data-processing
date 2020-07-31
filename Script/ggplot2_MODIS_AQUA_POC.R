
#MODIS AQUA Particulate Organic Carbon (POC)
#Tuan-Anh Bui
#Email: tuananh.bui@imbrsea.eu | anhbt95@gmail.com
#DEME | 2019-06-11


# Install and load prerequisite libraries ---------------------------------

install.packages(c("raster","ncdf4","tidyverse","sf","RColorBrewer"))

{ library(raster)
  library(ncdf4)
  library(tidyverse)
  library(sf)
  library(RColorBrewer)
}


# Check and change locale to the US to have international text display -------------

Sys.getlocale() #If not English, then change to English
Sys.setlocale("LC_ALL","English") 


# Load dataset ------------------------------------------------------------

##Load netCDF files 
# Change the directory - paste0("_change_this_part","/") - and name pattern - pattern="*_change_this_part" 
(f <- list.files(path = paste0("C:/Users/TABU/Desktop/wget/MODIS_Aqua_Monthly_POC_01.2006_04.2019","/"), pattern="*.L3m_MO_POC_poc_4km.nc",full.names=T))
#Note: Directory separation symbol of R "/" is not the same as of Windows "\"

#Explore the netCDF file
nc_open(f[1])

#Indicate variable, read from the netCDF file
var <- "poc"

##Convert netCDF files to StackRaster format
data_stack <- stack(f, varname = var) #Indicate variable as sst
data_stack

##Load shapefiles Add GSR and CCZ regions 
#Identify the directory of the shapefile - change the directory - paste0("_change_this_part","/")
SHP_dir <- paste0("C:/Users/TABU/Desktop/Tuan-Anh Bui/2_Deep sea/R_analysis/Data/GSR_coordinates/shapefiles","/")

#Identify name of shapefile files
GSR_name <- "GSR.shp" #Can use pattern to select different files
CCZ_name <- "CCZ.shp"

#Add GSR Shapefile - use st_read function of package sf
GSR <- st_read(paste0(SHP_dir,GSR_name))
#Add CCZ Shapefile
CCZ <- st_read(paste0(SHP_dir,CCZ_name))

##Crop area of interest
#Create extent for area of interest - GSR region
GSR #Load GSR to see the extent of the area
AOI <- extent(-129,-121,11,17) 

#Crop RasterStack by arae of interest
data_AOI <- crop(data_stack, AOI)

crs(data_AOI); crs(GSR) #Check to see if crs (Coordinate Reference System) of the 2 files are not the same
#In this case they are the same


# Create data frame with information of files in area of interest ---------
# In this script, ggplot2 package is used for visualization, thus input data must be dataframe

#Create an empty data frame 
df <- data.frame(year = integer(),
                 month = factor(),
                 lon = double(),
                 lat = double(),
                 value = double(),
                 unit = character(),
                 var = character (),
                 #Avoid the character vectors being converted to factors 
                 stringsAsFactors = F
) 

#Set up loop function to extract data to the data frame- use seq_along(f)
for (i in 1:length(f)) {
  # progress indicator
  print(paste("Processing file",i,"from",length(f),sep=" "))
  
  # open netCDF file
  data <- nc_open(f[i])
  
  # Extract information from netCDF file
  # Extract date
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year <- as.integer(format(datemean, "%Y"))
  month <- format(datemean, "%B")
  #Extract unit
  unit <- ncatt_get(data,var,"units")$value
  
  #Extract lon, lat, value from raster file (in arae of interest)
  df_raster <- as.data.frame(data_AOI[[i]], xy = T)
  
  #Creat temporary data frame
  df_temp <- data.frame(rep(year, nrow(df_raster)), #Duplicate year value by number of rows of df_raster,
                        rep(as.factor(month),nrow(df_raster)), 
                        df_raster,
                        rep(unit, nrow(df_raster)),
                        rep(var, nrow(df_raster)),
                        stringsAsFactors = F
  ) 
  names(df_temp) <- c("year","month","lon","lat","value","unit","var")
  df <- as_tibble(rbind(df, df_temp, stringsAsFactors = F))
  rm(data, dateini, dateend, datemean, year, month, df_raster, df_temp)
}
df

## Script to export data frame 
## Export data frame is not recommended
## Even in a small area, the size of file in .csv format would be extremely large

#Set the pathway to export data frame
#Export_path <- "C:/Users/TABU/Desktop/Tuan-Anh Bui/2_Deep sea/R_analysis/Data/Data frame/"
#Export data in .csv format
#write_excel_csv2(df, #data frame to export
#                 paste0(Export_path,"MODIS_AQUA_CHL.csv"), #path to save to
#                 delim = ";", #delimiter used to separate values 
#                 append = F, #Overwrite existing file
#                 col_names = T)


# Plot using ggplot (package ggplot2) -------------------------------------

#make a list of year in data for the loop 
year_list <- as.character(unique(df$year)) 
seq_along(year_list)

## Indicate min and max value of monthly chlorophyll a concentration to set range of legend
# Define monthly min and max value
df_min_max <- df %>%
  group_by(year,month) %>%
  summarize(value_min = min(value, na.rm = T),
            value_max = max(value, na.rm = T))

# Define min and max value to set range of legend
min(df_min_max$value_min, na.rm = T) #Min value - 13.4
max(df_min_max$value_max, na.rm = T) #Max value - 572.2
# Very large value of value_max compared to value_min 
# We may need the log scale for the legend


#Use frequency plot to explore the value_min distribution
ggplot(df_min_max) + geom_freqpoly(aes(value_min), binwidth = 1)
#No special distribution in min value

#Use frequency plot to explore the value_max distribution
ggplot(df_min_max) + geom_freqpoly(aes(value_max), binwidth = 1)
#The value is mostly under 300

# The binwidth value needs to be adjusted with respect to data value

# If we cannot clearly define the distribution of values from min_value and max_value, we can explore the whole dataset
# Explore the distribution of all values
#ggplot(df) + geom_freqpoly(aes(value), binwidth = 0.0000001)
#Zoom into 0-0.0005 as most the values are in this range 
#ggplot(df) + geom_freqpoly(aes(value), binwidth = 0.0000001) + 
#  coord_cartesian(c(0,0.0005))
#Zoom into 0-2e-04 as most the values are in this range 
#ggplot(df) + geom_freqpoly(aes(value), binwidth = 0.0000001) + 
#  coord_cartesian(c(0,2e-04))

#We will use log scale for the legend

#Set break of the legend based on the distribution of values
#Set break from value_min to value_max (We want to use log scale so do not set min as 0)
legend_break <- c(10, 20, 50, 100, 200, 600)


#Create set of colors 
#Use the reversed Spectral color palette (Blue - Red, center yellow) 
RLcolor <- rev(brewer.pal(11, "Spectral")) %>%
  colorRampPalette(bias=1,space="Lab",interpolate="linear")

#Use the Blue-Red, center green - color pallet 
#RLcolor <- colorRampPalette(c("royalblue","skyblue","green","yellow","red"),bias=1,space="Lab",interpolate="linear")

# Plot and export all the plot by yearly basis using loop function

##Note - Change the save directory at the end of the loop script
length(year_list)

for (i in 1:length(year_list)) { 
  #In case of testing before final export, the length(year_list) can be changed to 1 or 2
  
  #Progress indicator
  print(paste("Processing file",i,"/",length(year_list),"-",year_list[i],"data",sep=" "))
  
  #Select subset of dataframe to plot from
  df_subset <- subset(df, year == year_list[i]) 
  
  #Plot
  plot <- ggplot() +
    #Plot raster files 
    geom_raster(data = df_subset, 
                aes(x = lon, y = lat, fill = value)) +
    
    #Plot shapefile
    geom_sf(data = GSR, fill = "transparent", color = "black") +
    
    #Plot multiple months, can use function labeller to set labels
    facet_wrap(~ month,
               #Set number of columns
               ncol = 3,
               #Set number of rows
               nrow = 4) + 
    
    #Adjust lables
    labs(
      title = "MODIS AQUA Particulate Organic Carbon (POC)", #Title 
      subtitle = paste0("GSR contract areas ", year_list[i]), #Subtitle
      caption = "Author: DEME - Data: NASA OB:DAAC", #Caption
      x = NULL, 
      y = NULL
    ) +
    
    #Adjust legend
    scale_fill_gradientn(
      name = "Particulate Organic Carbon (mg/m³)", #Title of the legend
      label = as.character(legend_break), #label of the legend, must be the same lenght as breaks
      
      trans = "log10", #Transfer the scale of legend to logarithm
      breaks = legend_break, #Indicate the breaks of the legend
      limits = c(min(legend_break),max(legend_break)), #Indicate the limit of legend
      
      colours = RLcolor(20), #Color ramp of the legend
      na.value = "white", #Color of NA value
      
      guide = guide_colorbar(
        barheight = unit(0.6, units = "cm"), #Height of legend bar
        barwidth = unit(20, units = "cm"), #Width of legend bar
        label.position = "bottom", #Position of legend bar
        title.position = "top", #Position of title
        title.hjust = 0.5, #Position of title
        nrow=1
      )
    ) +
    
    #Adjust theme
    theme(plot.margin = unit(c(0.5,0.5,0.2,0.5), "cm"), #Adjust margin around the entire plot
          
          #Adjust title, subtitle, caption (size, and position)
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
          plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(0,0,30,0)),
          plot.caption = element_text(size = 14, hjust = 0.98, margin = margin(30,0,0,0)), 
          
          #Adjust facet panels
          panel.background = element_rect(fill = "white", color = "transparent"), #Adjust background of panel
          strip.background = element_rect(fill = "transparent", color = "transparent"),
          strip.text = element_text(size = 16, face = "bold"), #Adjust strip text (title of the panel)
          panel.spacing = unit(0, "cm"), #Adjust space between each panel
          axis.text = element_text(size = 14), #Adjust axis text
          
          #Adjust legnd
          legend.position="bottom", #Indicate position of legend
          legend.title = element_text(size = 16, margin = margin(10,0,0,0)),
          legend.text = element_text(size = 14), #Indicate size of legend
    ) 
  
  # save plots as .png - change the directory - path = paste0("_change_this_part","/") - filename = paste("_change_this_part")
  ggsave(path = paste0("C:/Users/TABU/Desktop/Tuan-Anh Bui/2_Deep sea/R_analysis/Figures/MODIS_AQUA_POC_2006_2019","/"),
         filename = paste("AQUA_MODIS_POC_",year_list[i],".png"), scale=2,
         plot = last_plot(),
         width = 16, height = 20, units = "cm",
         dpi = 300
  )
  
  # save plots as .pdf - change the directory - path = paste0("_change_this_part","/") - filename = paste("_change_this_part")
  # ggsave(path = paste0("C:/Users/TABU/Desktop/Tuan-Anh Bui/2_Deep sea/R_analysis/Figures/MODIS_AQUA_POC_2006_2019","/"),
  #       filename = paste("AQUA_MODIS_POC_",year_list[i],".pdf"), scale=2,
  #      plot = last_plot(),
  #      width = 14, height = 17, units = "cm",
  #      dpi = 300
  #      )
  
  # print plots to screen
  # print(plot)
  rm(df_subset)
}







