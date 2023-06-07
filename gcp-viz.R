
# Visualize Gross County Product
library(tidyverse)
library(ggplot2)
library(rgdal)
library(sf)
library(plyr)
library(leaflet)
library(RColorBrewer)

gc()
rm(list=ls())

# set the working directory to the current folder
setwd('/home/ken/Desktop/knbs-gcp')
# load in the gcp dataset
gcp_df <- read.csv('datasets/gcp-current.csv', header=TRUE)
gcp_df <- gcp_df[c(1:47),]
names(gcp_df) <- c("COUNTY","YEAR_2013","YEAR_2014","YEAR_2015","YEAR_2016","YEAR_2017","YEAR_2018","YEAR_2019","YEAR_2020")
View(gcp_df)

# calculate average gcp per county for years from 2013 to 2020
gcp_df <- gcp_df %>% 
  mutate(gcp_mean=rowMeans(select(gcp_df, starts_with("YEAR_"))))

# load in the shapefile
shp <- readOGR(dsn="kenyan-counties", layer="County")
plot(shp)

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

# use ggplot to visualize contribution of gcp for various counties
ggplot() +
  geom_polygon(data=kenya_df, aes(long, lat, group=group, fill=log(gcp_mean)), colour="grey", linewidth=0.5) + 
  geom_path(data=kenya_df, aes(long, lat, group=group), color="black", linewidth=0.5) +
  scale_fill_distiller(name="Log GCP", palette="YlOrBr")+
  labs(title="Gross County Product Map", subtitle="Average GCP for years 2013 - 2020", caption="Use Natural Log of the GCP to minimize effect of disparity") +
  theme_void() +
  theme(
    plot.title = element_text(hjust=0.5, size=17, face="bold"), 
    plot.caption = element_text(color="#edae49", size=13, hjust=0.5, face="italic"),
    plot.subtitle = element_text(vjust=0.5, hjust=0.5, size=15, face="italic", family="Helvetica", color="brown"))

# create a basic leaflet object to visualize county borders
leaflet() %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas, group="Default Maptile", options=providerTileOptions(noWrap=TRUE)) %>%
  fitBounds(33.97, -4.471, 41.85688, 3.93726) %>%
  setMaxBounds(32, -3.9, 43, 4.5) %>%
  setView(lng=37.9062, lat=1.00, zoom=6) %>% 
  addPolygons(data=shp, col="blue", weight=1, layerId = ~id, label=~COUNTY)

# use a continuous color palette
#paletteNum <- colorNumeric("Greens",domain=gcp_df$YEAR_2013)
qpal <- colorQuantile(rev(viridis::viridis(25,option="G")),
                      shp$YEAR_2013, n=25)
# create text for county labels
mylabels <- paste(
  "County: ", shp$COUNTY,"<br/>",
  "GCP: ", prettyNum(shp$YEAR_2013, big.mark=",")
) %>% lapply(htmltools::HTML)

# plot title
plot_title <- "<h4>Gross County Product (GCP) For Year 2013, in Kshs Million</h4>"
# FINAL MAP
leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group="Default Maptile", options=providerTileOptions(noWrap=TRUE)) %>%
  fitBounds(33.97, -4.471, 41.85688, 3.93726) %>%
  setMaxBounds(32, -3.9, 43, 4.5) %>%
  setView(lng=37.9062, lat=1.00, zoom=6) %>%
  addPolygons(
    data=shp, 
    col="whitesmoke", 
    weight=1,
    label=mylabels,
    labelOptions = labelOptions(
      style=list("font-weight"="bold", padding="3px 8px"),
      direction="auto"
    ),
    layerId = ~id, 
    fillOpacity=1, 
    fillColor=~qpal(YEAR_2013),
    highlightOptions = highlightOptions(
      color="#000000", weight=2,
      bringToFront=TRUE, sendToBack = TRUE
    )
    ) %>% 
  addControl(html=plot_title, position="topright")
