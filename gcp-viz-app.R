library(shiny)
library(sf)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(plyr)
library(leaflet) 
library(RColorBrewer)

setwd('/home/ken/Desktop/knbs-gcp')

# load county shapefile
shp <- readOGR(dsn="kenyan-counties", layer="County")
#plot(shp)
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
#View(kenya_df )
ui <- fluidPage(
  # CSS
  tags$head(
    tags$style(HTML("
    body{
      background-color: #FFE3C3;
    }
    h2 { text-align: center; 
    color: #641800; 
    font-weight:bold; 
    text-decoration: underline black 5px;
    text-underline-offset: 0.3em;
    margin: 12px 7px; 
    padding: 5px 5px;
    }
    .leaflet{ border: 1px solid brown; border-radius: 15px 30px;}
    .well{
      background: #7FFFCE;
    }
    #ul-green{
     text-decoration: underline green 3px;
     text-underline-offset: 0.3em;
    }
    #ul-red{
     text-decoration: underline red 3px;
     text-underline-offset: 0.3em;
    }
    #ul-black{
     text-decoration: underline black 3px;
     text-underline-offset: 0.3em;
    }
    .info.legend{
     background: #f8e6d1;
     border: 1px solid #236931;
    }
      "))
    ),
  titlePanel("Gross County Product (GCP) 2021"),

    sidebarLayout(
      sidebarPanel(
        selectInput("Year",
                    "Select Year",
                    choices=inputChoices,
                    selected=inputChoices[1]),
        br(),
        sliderInput("numColors",
                    "Number of Colors",
                    min=5,
                    max=45,
                    step=5,
                    value=10),
        br(),
        selectInput("colorPal",
                    "Change Color",
                    choices=c("turbo","mako","plasma","viridis","magma"),
                    selected="turbo")
      ),
      mainPanel(
        leafletOutput("country_map", height=600)
        #plotOutput("ggplotMap", height=600)
      )
    )
  
)
  
  
server <- function(input, output){
  # input year selected by user
   selectYear <- reactive({
     selected_year <- input$Year
   })
   
  # number of colors selected by user using sliderInput
   numColors <- reactive({
     num_colors <- input$numColors
   })
   
   # color chosen
   colorChosen <- reactive({
     # reset action button value to 0 when it reaches 7
    color_chosen <- input$colorPal
   })
   
   output$country_map <- renderLeaflet({
     selected_year <- selectYear()
     num_colors <- numColors()
     color_chosen <- colorChosen() 
     # generate color palette
     cool_lst1 <- reactive({
       #kenya_df %>% select(selected_year)
       eval(str2expression(paste0('shp','$',selected_year)))
     })
     qpal <- colorQuantile(rev(viridis::viridis(num_colors,option=color_chosen)),
                           cool_lst1(), n=num_colors)
     # hover labels
     mylabels <- paste(
       "County: ", shp$COUNTY,"<br/>",
       "GCP: ", prettyNum(eval(str2expression(paste0('shp','$',selected_year))), big.mark=",")
     ) %>% lapply(htmltools::HTML)
     
     # plot title
     plot_title <- paste("<h4><span id='ul-green'>Gross County Product (GCP) </span> <span id='ul-red'>For Year", selected_year, "</span> <span id='ul-black'>in Kshs Million</span></h4>")
     
     # references
     references <- paste("<div class='references'> <h5 style='font-weight: bold;'>References: </h5>",
                         "<h5>Visualization by:",
                         "<a target='_blank' href='https://www.linkedin.com/in/kennedy-waweru/'>Kennedy N Waweru</a></h5>",
                         "<h5>Source Code:",
                         "<a target='_blank' href='https://github.com/KennedyWaweru/KNBS_GrossCountyProduct_DataViz'>Github</a></h5>",
                         "<h5>Data Source:",
                         "<a target='_blank' href='https://www.knbs.or.ke/download/gross-county-product-gcp-2021/'>KNBS</a></h5>",
                         "</div>")
     # final map
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
         fillColor=~qpal(cool_lst1()),
         highlightOptions = highlightOptions(
           color="#000000", weight=2,
           bringToFront=TRUE, sendToBack = TRUE
         )
       ) %>% 
       addControl(html=plot_title, position="topright") %>%
       addControl(html=references, position="bottomleft")
     
   }
   )


   # output ggplot map works well but not easy to contrast values for different years
   # output$ggplotMap <- renderPlot({
   #   selected_year <- selectYear()
   #   ggplot() +
   #     geom_polygon(data=kenya_df, aes(long, lat, group=group, fill=log(eval(str2expression(selected_year)))), colour="grey", linewidth=0.5) + 
   #     geom_path(data=kenya_df, aes(long, lat, group=group), color="black", linewidth=0.5) +
   #     scale_fill_distiller(name="Log GCP", palette="YlOrBr")+
   #     labs(title="Gross County Product Map", subtitle="Average GCP for years 2013 - 2020", caption="Use Natural Log of the GCP to minimize effect of disparity") +
   #     theme_void() +
   #     theme(
   #       plot.title = element_text(hjust=0.5, size=17, face="bold"), 
   #       plot.caption = element_text(color="#edae49", size=13, hjust=0.5, face="italic"),
   #       plot.subtitle = element_text(vjust=0.5, hjust=0.5, size=15, face="italic", family="Helvetica", color="brown"))
   #   
   # })
}  

shinyApp(ui, server)