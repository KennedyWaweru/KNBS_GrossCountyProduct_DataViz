
# Define server logic required to draw a barplot
function(session, input, output) {
  
  selectSector <- reactive({
    select_sector <- input$sector
  })
  
  # logic for showing top 5 counties
  showTop <- reactive({
    show_top <- input$top_5
    
  })
  
  observe({
    if(input$top_5){
      # update select input to top 5 per selected sector
      new_df <- dfSubset()
      top_5 <- new_df$County[1:5]
      updateSelectizeInput(session,"county",choices=new_df$County, selected=top_5)
    }else{
      # default select input for county when not showing top 5
      updateSelectizeInput(session,"county",
                           choices=df$County,
                           selected="NAIROBI",
                           options=list(maxItems=16))
    }
  })
  
  selectCounty <- reactive({
    select_county <- input$county
  })
  
  
  output$distPlot <- renderPlot({
    #selected_year <- selectYear()
    selected_sector <- selectSector()
    selected_county <- selectCounty()
    
    if(length(selected_county)>1){
      filter_condition <- paste0("County=='",selected_county, collapse="|",sep="'")
    }else{
      filter_condition <- paste0("County=='",selected_county,collapse="|",sep="'")
    }
    
    plot_title <- stringr::str_to_title(gsub("_"," ",selected_sector))
    (
      # use facet wrap to show subplots if num selected counties is more than 6
      if(length(selected_county)>5){
        p <- 
          df %>% filter(eval(str2expression(filter_condition))) %>%
          ggplot() +
          geom_col(aes(x=Year, y=eval(str2expression(selected_sector)),fill=Year), position=position_dodge2()) +
          labs(title=plot_title, y="GCP, Kshs Million", x="Year") +
          scale_fill_brewer(palette = "Oranges") +
          scale_y_continuous(labels=comma) +
          facet_wrap(~County, scales="free_y") +
          theme_bw()
      }else{
        p <- 
          df %>% filter(eval(str2expression(filter_condition))) %>%
          ggplot() +
          geom_col(aes(x=County, y=eval(str2expression(selected_sector)),fill=Year), position=position_dodge2()) +
          labs(title=plot_title, y="GCP, Kshs Million", x="Year") +
          scale_fill_brewer(palette = "Oranges") +
          scale_y_continuous(labels=comma) +
          theme_linedraw()
      }
    )
    p
  })
  
  dfSubset <- reactive({
    selected_sector <- selectSector()
    df_subset <- df %>% select(County, selected_sector, Year)
    df_sorted <- df_subset %>% 
      pivot_wider(names_from=Year, names_glue="Year_{Year}", values_from=selected_sector) %>% 
      mutate(total_gcp = (Year_2018+Year_2019+Year_2020)) %>% 
      arrange(desc(total_gcp))
    
    df_sorted
  })
  # show sorted table
  output$my_table <- DT::renderDataTable(
    dfSubset(),
    options = list(
      pageLength=10
    )
  )
  
  # CODE FOR INTERACTIVE APP VISUALIZATION (MAP)
  # ---------------------------------------------------------------------------------------------------
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
    if(selected_year=="gcp_mean"){
      title_year <- "2013-2020 (Mean)"
    }else{
      title_year <- gsub("YEAR_","",selected_year)
    }
    
    plot_title <- paste("<h4><span id='ul-green'>GCP For</span> <span id='ul-red'>Year", title_year, "</span> &nbsp; <span id='ul-black'>  in Kshs Million</span></h4>")
    
    # references
    # references <- paste("<div class='references'> <h5 style='font-weight: bold;'>References: </h5>",
    #                     "<h5>Visualization by:",
    #                     "<a target='_blank' href='https://www.linkedin.com/in/kennedy-waweru/'>Kennedy N Waweru</a></h5>",
    #                     "<h5>Source Code:",
    #                     "<a target='_blank' href='https://github.com/KennedyWaweru/KNBS_GrossCountyProduct_DataViz'>Github</a></h5>",
    #                     "<h5>Data Source:",
    #                     "<a target='_blank' href='https://www.knbs.or.ke/download/gross-county-product-gcp-2021/'>KNBS</a></h5>",
    #                     "</div>")
    
    references <- paste("<div>",
                        "<h5>Visualization by:",
                        "<a target='_blank' href='https://www.linkedin.com/in/kennedy-waweru/'>Kennedy N Waweru</a></h5>",
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
  
}

  