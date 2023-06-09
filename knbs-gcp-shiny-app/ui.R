# Define UI for application that allows user to select variables
navbarPage(title="GCP VIZ APP",
           theme = bs_theme(bootswatch = "flatly"),
           
  # Tab for GCP by Economic Sector Comparison
  # -----------------------------------------------------
  
  # CSS
  tags$head(
    tags$style(HTML("
    body{
      background-color: #FFE3C3;
    }
    h2 { text-align: center; 
    color: #641800; 
    font-weight:bold; 
    text-decoration: underline black 3px;
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
    .centerFigure img{
     display: block;
     margin-right: auto;
     margin-left: auto;
     border: 1px solid green;
     border-radius: 50%;
    }
    .centerFigure figcaption{
      text-align: center;
    }
    hr{
     border: 1px solid #35c4a8;
    }
      "))
  ),
  
  tabPanel(title="GCP by Sector",
           fluidPage(
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
               card(
                 card_header(
                   class = "bg-dark",
                   h4("Table Sorted to show Top Counties on Top",align="center")
                 ),
                 DT::dataTableOutput("my_table")
               )
             )
             
           ) # END OF FLUID PAGE
           ), # END OF ECONOMIC SECTOR TAB
       
  # TAB FOR INTERACTIVE MAP
  # ---------------------------------------------------------------
  tabPanel(title="Interactive Country Map",
           fluidPage(
             
             titlePanel("Gross County Product (GCP) Report"),
             
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
             
           ) # END OF FLUID PAGE FOR INTERACTIVE MAP PAGE
        ), # end of tab panel for second page
  
  #----------------------
  # add tab panel to show references 
  tabPanel(title="References",
           card(
             card_header(
               class = "bg-primary",
               h4("Project References",align="center")
             ),
             #markdown("Some text with a [link](https://github.com).")
             # Create a html table to show references
             tags$table(
               tags$tr(
                 tags$th("Resource"),
                 tags$th("Link")
               ),
               tags$tr(
                 tags$td("Data Source"),
                 tags$td(
                   markdown("<a target='_blank' href='https://www.knbs.or.ke/download/gross-county-product-gcp-2021/'>KNBS</a>")
                 )
               ),
               tags$tr(
                 tags$td("Source Code"),
                 tags$td(
                   markdown("<a target='_blank' href='https://github.com/KennedyWaweru/KNBS_GrossCountyProduct_DataViz'>Github</a>")
                 )
               ),
               tags$tr(
                 tags$td("Tutorial Article"),
                 tags$td(
                   markdown("<a target='_blank' href='https://dasclab.uonbi.ac.ke/analytics/blog/visualizing-map-data-with-ggplot-and-leaflet'>DASCLAB Blog</a>")
                   )
               ),
               tags$tr(
                 tags$td("About Author"),
                 tags$td(
                   markdown("<a target='_blank' href='https://www.linkedin.com/in/kennedy-waweru/'>Kennedy N Waweru</a>")
                 )
               )
             )
           ),
           br(),
           card(
             card_header(
               class="bg-primary",
               h4("Author Bio",align="center")
             ),
             # image that is centered
             tags$figure(
               class = "centerFigure",
               tags$img(
                 src = "kennedy-waweru.jpg",
                 width = 200,
                 height = 200,
                 alt = "Picture of Project Author"
               ),
               tags$figcaption("Image of Project Author",align="center"),
               br(),
               p("Motivated machine learning scientist adept at regression and classification, web scraping, data analysis and cloud deployment.
                Possessing a Bachelor's degree in Actuarial Science, I am adept at leveraging data to drive informed decision-making and enhance business performance. 
                I am well-versed in utilizing statistical software packages such as Python and R to extract actionable insights from complex data sets.
                I offer a strong foundation in Data Science, a passion for problem-solving, and a commitment to delivering actionable insights."),
               hr(),
               h3("Read My Blog", align="center"),
               p("I invite you to dive into our thought-provoking blog that explores the fascinating topics of data analytics and machine learning.
                 Whether you're interested in understanding the foundations, exploring real-world applications, or diving deep into complex algorithms, this blog has something for everyone.
                 "),
               br(),
               markdown("Visit <a href='https://dasclab.uonbi.ac.ke/analytics/blog' target='_blank'>dasclab.uonbi.ac.ke/analytics/blog</a> today and prepare to be inspired! Happy reading!")
             )
           )
      )
  )
