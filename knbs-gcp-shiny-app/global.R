# load in shiny package
library(shiny)
# load in bslib for bootstrap style themes
library(bslib)

# Packages for data visualization
library(ggplot2)
library(leaflet) 
library(RColorBrewer)
library(scales)

# for data wrangling
library(dplyr)
library(plyr)
library(tidyr)

# read in the shapefile
library(rgdal)

# for showing the table
library(DT)


# setwd('/home/ken/Desktop/knbs-gcp')

df <- read.csv("datasets/gcp_economic_sectors.csv")
df$Year <- as.factor(df$Year)


# CODE FOR INTERACTIVE APP
#---------------------------------------------------

# load county shapefile
shp <- readOGR(dsn="datasets/kenyan-counties", layer="County")

# load gcp dataset
gcp_df <- read.csv('datasets/gcp-current.csv', header=TRUE)
gcp_df <- gcp_df[c(1:47),]
names(gcp_df) <- c("COUNTY","YEAR_2013","YEAR_2014","YEAR_2015","YEAR_2016","YEAR_2017","YEAR_2018","YEAR_2019","YEAR_2020")


# calculate average gcp per county for years from 2013 to 2020
gcp_df <- gcp_df %>% 
  mutate(gcp_mean=rowMeans(select(gcp_df, starts_with("YEAR_"))))


# convert shapefile county names to uppercase to match KNBS dataset
shp$COUNTY <- stringr::str_to_upper(shp$COUNTY)

# inspect to ensure shapefile county names match gcp dataset county names
setdiff(shp$COUNTY, gcp_df$COUNTY)

# inconsistencies: KEIYO-MARAKWET, UASIN GISHU, THARAKA, MURANG'A
marakwet_id <- which(shp$COUNTY=="KEIYO-MARAKWET")
shp$COUNTY[marakwet_id] <- "ELGEYO MARAKWET"

tharaka_id <- which(shp$COUNTY=="THARAKA")
shp$COUNTY[tharaka_id] <- "THARAKA NITHI"

muranga_id <- which(shp$COUNTY=="MURANG'A")
shp$COUNTY[muranga_id] <- "MURANGA"

# inspect to ensure that all county names match in both shapefile and the knbs dataset
setdiff(shp$COUNTY, gcp_df$COUNTY)
shp@data$id <- rownames(shp@data)
shp@data <- join(shp@data, gcp_df, by="COUNTY")

shp_df <- fortify(shp)
kenya_df <- join(shp_df, shp@data, by="id")
inputChoices <- rev(names(select(kenya_df, YEAR_2013:gcp_mean)))