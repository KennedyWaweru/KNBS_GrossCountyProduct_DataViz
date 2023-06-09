library(ggplot2)
library(scales)
library(shiny)
library(dplyr)
library(tidyr)

setwd('/home/ken/Desktop/knbs-gcp')

df <- read.csv("datasets/gcp_economic_sectors.csv")
df$Year <- as.factor(df$Year)
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("GCP Contribution by Economic Sector App"),
  fluidRow(
    
    column(5,
            selectInput("sector",
                        "Economic Sector:",
                        choices=colnames(df)[-c(1,length(colnames(df)))],
                        selected=names(df)[2])
            ),
     column(5,
            selectizeInput("county",
                           "Select One or More Counties:",
                           choices=df$County,
                           selected="NAIROBI",
                           multiple=TRUE,
                           options=list(maxItems=16))
            ),
     column(2,
            checkboxInput("top_5", "Select Top 5", FALSE))
   ),
   
   # plot output row
   fluidRow(
     plotOutput("distPlot", height="500px"),
     br(),
     h4("Table Sorted to show Top Counties on Top"),
     dataTableOutput("my_table")
   )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    selectSector <- reactive({
      select_sector <- input$sector
    })
    
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
            ggplot(aes(x=County, y=eval(str2expression(selected_sector)),fill=Year)) +
            geom_col(position=position_dodge2(width=0.9)) +
            geom_text(aes(x=County, y=eval(str2expression(selected_sector)), label=prettyNum(eval(str2expression(selected_sector)),big.mark=",")), position=position_dodge(width=1), vjust=-0.5, angle=-90, colour="black", fontface="bold") +
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
    output$my_table <- renderDataTable(
      dfSubset(),
      options = list(
        pageLength=10
      )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
