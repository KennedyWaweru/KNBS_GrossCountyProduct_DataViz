library(ggplot2)
library(shiny)
library(dplyr)

setwd('/home/ken/Desktop/knbs-gcp')

df <- read.csv("datasets/gcp_economic_sectors.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(
   fluidRow(
     # Application title
     titlePanel("GCP App")
   ),

    fluidRow(
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectInput("sector",
                      "Economic Sector:",
                      choices=colnames(df)[-1],
                      selected=names(df)[2]),
          br(),
          selectInput("county",
                      "Select First County:",
                      choices=df$County,
                      selected="NAIROBI",
                      multiple=TRUE),
          # br(),
          # selectInput("county2",
          #             "Compare with different County",
          #             choices=df$County,
          #             selected=df$County[2]),
          # br(),
          # selectInput("year",
          #             "Year of Comparison",
          #             choices=c(2018,2019,2020),
          #             selected=2018)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          textOutput("countySelect"),
          fluidRow(plotOutput("distPlot", height="600px"))
          
          #textOutput("varSelect")
        )
      )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # selectYear <- reactive({
    #   selected_year <- input$year
    # })
    
    selectSector <- reactive({
      select_sector <- input$sector
    })
    
    selectCounty <- reactive({
      select_county <- input$county
    })
    
    
    # comparisonCounty <- reactive({
    #   select_comparison <- input$county2
    # })
    # 
    output$countySelect <- renderText({
      selected_county <- selectCounty()
      if(length(selected_county)>1){
        filter_condition <- paste0("County=='",selected_county, collapse="|",sep="'")
      }else{
        filter_condition <- paste0("County=='",selected_county,collapse="|",sep="'")
      }
      filter_condition
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
    
      to_display  <- df %>% filter(eval(str2expression(filter_condition)))
      (
      p <- 
        # ggplot(to_display) +
        # geom_col(aes(x=as.factor(Year), y=selected_sector, fill=County), position=position_dodge()) 
          
          df %>% filter(eval(str2expression(filter_condition))) %>%
          ggplot() +
          geom_col(aes(x=County, y=Manufacturing,fill=as.factor(Year)), position=position_dodge2()) +
          labs(title="Agriculture Forestry and Mining", y="GCP, Kshs Million", x="Year") +
          theme_linedraw()
      )
     p
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
