library(shinydashboard)
library(shinyjs)

dashboardPage(skin = "blue",

  dashboardHeader(title = span(tagList(icon("line-chart"), "SReviews"))),
  
  dashboardSidebar(
      sidebarMenu(
      
        sidebarSearchForm(textId = "searchm", buttonId = "searchButton",label = "Enter movie name..."),
      
        menuItem("Result", tabName = "dashboard", icon = icon("th")),
        
        
        textOutput("fn"),
            
        tags$head(tags$style("#fn{color:white;font-size:15px;text-align:center;font-style:bold;
                             text-transform:uppercase;font-family:Times New Roman}")),   
        
        
        img(src="./image/side.jpg",height=500,width="100%"),
        br(),
        img(src="./image/side.jpg",height=500,width="100%"),
        br(),
        img(src="./image/side.jpg",height=500,width="100%"),
        br(),
        img(src="./image/side.jpg",height=500,width="100%")
                )
        ),

  dashboardBody(
    

    tabItems(  
          tabItem(tabName = "dashboard",
           
              ("Movie name:"),textOutput("name"),
              
              tags$head(tags$style("#name{color:black;font-size:40px;font-style:bold;
                                   text-transform:uppercase;font-family:Elephant}")),   
              br(),
              
              
              fluidRow(
                valueBoxOutput("instabox",width = 3),
                
                valueBoxOutput("youtubebox",width = 3),
                
                valueBoxOutput("IMDB rating",width = 3),
              
                  valueBoxOutput("fb",width = 3)
              ),
              
              
              fluidRow(
                  box(width=5,background = "black",uiOutput("poster")),
                  box(title = "Movie details:",solidHeader = TRUE,
                    status = "primary",  collapsible = TRUE, width=7,
                
                  ("Description:"),textOutput("description"),
                    br(),
                  
                  ("Director:"),textOutput("director"),
                    br(),
                  
                  ("Writers:"),textOutput("writers"),
                    br(),
                  
                  ("Star Cast:"),textOutput("stars"),
                    br()
                  )
                ),
              
          
              fluidRow(
                box( title="Trailer", solidHeader = TRUE,status = "primary",  background = "black",width=9,
                     uiOutput("video")),
              
           fluidRow(
             solidHeader = TRUE,
             status = "primary",  collapsible = TRUE,
             
            
             box(
               
               solidHeader = TRUE,
               status = "primary",  collapsible = TRUE,
               
               title=span(tagList(icon("youtube"), "Youtube")) ,width=3,
               span(tagList(icon("youtube-play"))),textOutput("vc"),
              br(),
              
              span(tagList(icon("thumbs-up"))),textOutput("ylike"),
              br(),
              
              span(tagList(icon("thumbs-down"))),textOutput("ydlike"),
              br(),
              
              span(tagList(icon("comment"))),textOutput("ycm"))
           )  
          ),
          
          fluidRow(
            tabBox(title=tagList(icon("twitter"),"Twitter Analysis"),id="tabset1",width=12,height = "400px",
                          tabPanel("sentiment analysis",plotOutput("piechart")),
                          tabPanel("Top tweets",tableOutput("tabledata")),
                          tabPanel("Wordcloud",plotOutput("word"))
                   
                          )),
            br(),
            br(),
            tags$h3("*Note: Reffered top 100 tweets only"),
            br(),
          
        
      
          fluidRow(tabBox(title=tagList(icon("google"),"Google Trends"),id="tabset2",width=12,height = "300px",
                          tabPanel("Search Intrest",uiOutput("gt"))
                      
                        ))
          
        
           
     )
     )
    
     
    
)
)