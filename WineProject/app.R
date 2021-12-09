library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(plotly)

setwd('~/R/')
data <-read.csv("./WineData.csv")
#Projects/WineProject/WineData.csv
chmap2 <- read.csv("./ChloroMapData.csv")
#Projects/WineProject/ChloroMapData.csv
full <- read.csv("./WineWords.csv")
#Projects/WineProject/WineWords.csv
full <- full %>%
  rename(., "Very Good" = "Very.Good")

ch_map_df <-data %>%
  group_by(., province, country, Rating) %>%
  summarise(., Count = length(Rating))%>%
  spread(., Rating, Count) %>%
  #unite(., col = "province", province, country, sep= ", ") %>%
  replace_na(., list(Good = 0,
                     `Very Good` = 0,
                     Outstanding = 0,
                     Classic = 0)) %>%
  mutate(., Total = Good + `Very Good` + Outstanding, Classic)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 10px}"),
          tags$style(".main-header .logo {height: 50px}")),
  title = "Wine Statistics ",
  titleWidth = 250)  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(id = 'tabs',
    menuItem(" Production", tabName = "map", icon = icon("map")),
    menuItem(" Ratings", tabName = 'charac', 
             icon = icon("area-chart")),
    menuItem("Dataset", tabName = "data", icon = icon("database"))
  )
)



# combine the two fluid rows to make the body
body <- dashboardBody(
  tags$head(tags$style(HTML('
                                /* logo */
                            .skin-blue .main-header .logo {
                                    font-weight=bold;  
                                    font-size: 28px;                            
                                  background-color: maroon;
                            }
                            .span12 {background-color: #000000
                            }
                             .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper{
                             background: -webkit-linear-gradient(maroon , black); 
                             }
                             .skin-blue .main-header .navbar{
                             background: -webkit-linear-gradient(left, maroon, black); 
                            }
                            .skin-blue .main-header .logo:hover{
                            background-color: maroon;
                            }
                            .content {
                              background-color: silver ;}
                            .content {
                              min-height: 100%;
                            }

                            /* navbar , (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #000000;
                            }        
                            
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #7f355b;
                            }e
                            
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #5e0a54;
                            }
                            
                            /* other links in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu wrapper{
                            background: -webkit-linear-gradient(black, red)
                            # background-color: #7f355b;
                            # color: #000000;
                            }

                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                font-family: "Balto";
                                 font-size: 20px; 
                            }
                            
                            /* other links in the sidebarmenu when hovered */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: gray;
                            }
                            /* toggle button when hovered  */                    
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #7f355b;
                            }
                            ')
                           )),
  tabItems(
    tabItem(tabName = "map",
            br(),
            br(),
            fluidRow(
            box(width = 8,
                plotlyOutput("map")),
            br(), br(), br(), br(),
            box(width = 4,
              tags$h2(tags$b("Wine Spectator's 100-Point Scale")),
                      (tags$div(tags$p("95-100 Classic: a great wine", tags$br(),
                         "90-94 Outstanding: a wine of superior character and style",tags$br(),
                         "85-89 Very good: a wine with special qualities", tags$br(),
                         "80-84 Good: a solid, well-made wine", tags$br(),
                         "75-79 Mediocre: a drinkable wine that may have minor flaws",tags$br(),
                         "50-74 Not recommended"))))),
               
            br(),
          fluidRow(
            tags$div(tags$style(HTML('.box{box-shadow: 10px 10px 5px grey;}
                                      #prov {font-size: 25;}'))),
             box(selectInput("prov", "View Country Breakdown",
                            choices = unique(data$country),
                            selected = "Portugal"),
                width = 6,
                visible = FALSE,
                plotlyOutput("province")),
            box(width = 6, 
                visible = FALSE,
                plotlyOutput("pricedis"))
            )),
    tabItem(tabName = "charac",
            br(),
            h1("What does it take to produce superb wine?"),
            h3("Use this tool to visualize the most commonly used words to describe wine based on their rating. "),
            hr(),
            fluidRow(
            column(width = 8,
            selectInput("rate", 
                        "Select Rating:",
                        choices = unique(data$Rating)),
            box(width = NULL,
                plotOutput("words"))),
            
              br(), br(), br(), br(),
              valueBoxOutput("value1"),
              valueBoxOutput("value2"),
              valueBoxOutput("value3"))
          ),
    tabItem(tabName = "data",
            br(), br(),
            fluidRow(box(width = 12,
              DT::dataTableOutput("datat"))),
            uiOutput("tab")
             
    
)))

#completing the ui part with dashboardPage
ui <- dashboardPage(header, sidebar, body)

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  val1 <- reactive ({
    data %>%
      filter(., Rating == input$rate) %>%
      summarise(., Avg = mean(price, na.rm = TRUE)) %>%
      round(., 2)
  })
  
  ct <- reactive ({
    data %>%
      filter(., Rating == input$rate)%>%
      summarise(., Counts = length(Rating)) %>%
      mutate(., Per = (Counts/nrow(data))*100) %>%
      select(., Per) %>%
      round(., 2)
  })
  pop <- reactive ({
    data %>%
      filter(., Rating == input$rate) %>%
      group_by(., variety) %>%
      summarise(., Len = length(variety)) %>%
      arrange(., desc(Len)) %>%
      select(., variety) %>%
      head(n=1)
      
  })
  # TDM <- reactive ({
  #   full %>%
  #     arrange(., desc(input$rate, na.rm = TRUE)) %>%
  #     select(., name, input$rate) %>%
  #     head(n = 18)
  # })
  
  pro <- reactive({
    ch_map_df %>%
      arrange(., desc(Total)) %>%
      filter(., country == input$prov) %>%
      head(n = 5)
  })
  
  f1 <- list(
    family = "Arial, sans-serif", 
    size = 18,
    color = "black")
  
  f2 <- list(
    family = "Arial, sans-seiff",
    size = 14,
    color = "black"
  )
  output$words <- renderPlot({
    if (input$rate == "Outstanding"){
            x <- full %>%
                arrange_(., 'desc(Outstanding)') %>%
                 select_(., 'name', 'Outstanding') %>%
                  head(n = 18)
                ggplot(x, aes_string(x=paste0("reorder(",'name',", Outstanding)"), y = 'Outstanding')) + 
                geom_bar(stat = 'identity', aes(fill = name)) + 
                 xlab("Words") + ylab("Count") +
                theme(axis.text.x = element_text(angle =45, 
                                         hjust =  1),
                      text = element_text(size=  18))
                
                
      } else if (input$rate == "Classic"){
            x <- full %>%
              arrange_(., 'desc(Classic)') %>%
              select_(., 'name', 'Classic') %>%
              head(n = 18)
            ggplot(x, aes_string(x=paste0("reorder(",'name',", Classic)"), y = 'Classic')) + 
              geom_bar(stat = 'identity', aes(fill = name)) + 
              xlab("Words") + ylab("Count") +
              theme(axis.text.x = element_text(angle =45, 
                                               hjust =  1),
                    text = element_text(size = 15))
            
            
           } else if (input$rate == "Good"){
              x <- full %>%
                arrange_(., 'desc(Good)') %>%
                select_(., 'name', 'Good') %>%
                head(n = 18)
             ggplot(x, aes_string(x=paste0("reorder(",'name',", Good)"), y = 'Good')) + 
               geom_bar(stat = 'identity', aes(fill = name)) + 
               xlab("Words") + ylab("Count") +
               theme(axis.text.x = element_text(angle =45, 
                                                hjust =  1),
                     text = element_text(size = 18))
             
             }else {
             x <-   full %>%
                arrange_(., 'desc(`Very Good`)') %>%
                select_(., 'name', '`Very Good`') %>%
                head(n = 18)
               ggplot(x, aes_string(x=paste0("reorder(",'name',", `Very Good`)"), y = '`Very Good`')) + 
                 geom_bar(stat = 'identity', aes(fill = name)) + 
                 xlab("Words") + ylab("Count") +
                 theme(axis.text.x = element_text(angle =45, 
                                                  hjust =  1),
                       text = element_text(size =18))             } 
    
    # ggplot(VDM, aes_string(x=paste0("reorder(",'name',", input$rate)"), y = 'input$rate')) + 
    #   geom_bar(stat = 'identity', aes(fill = name)) + 
    #   xlab("Words") + ylab("Count") +
    #   theme(axis.text.x = element_text(angle =45, 
    #                                    hjust =  1))
    })
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
       paste("$", val1())
      ,paste('Average Selling Price of ', input$rate, 'Wines' )
      ,icon = icon("usd",lib='glyphicon')
      ,color = "maroon")
    
    
  })
  
  output$value2 <- renderValueBox({
      valueBox(
         paste(ct(),'%')
        ,paste("Percentage of Total Wines")
        ,icon = icon("list",lib='glyphicon')
        ,color = "maroon")
      
      
    })
  
  output$value3 <- renderValueBox({
    valueBox(pop()
      ,paste("Most Popular Blend of", input$rate,
             " Wines")
      ,icon = icon("thumbs-up",lib='glyphicon')
      ,color = "maroon")
    
    
  })
  
  output$province <- renderPlotly({
  
    pr <- plot_ly(pro(), x = ~province,  y = ~Classic,
                  name = "Classic",
                  type = 'bar') %>%
      add_trace(y = ~Outstanding, name = "Outstanding") %>%
      add_trace(y = ~`Very Good`, name = "Very Good") %>%
      add_trace(y = ~Good, name = "Good") 


    ggplotly(pr) %>%
      layout(autosize = T,
             xaxis = list (title = ~country,
                           showticklabels = FALSE,
                           zeroline = TRUE,
                           showline = TRUE,
                           tickangle = 45,
                           titlefont = f2),
             yaxis = list(title = "Count",
                          zeroline = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          showgrid = FALSE,
                          titlefont = f2),
             barmode = 'group',
             paper_bgcolor=' white',
             plot_bgcolor = 'white',
             hoverlabel = list(size = 30))
  })
    
  
  output$pricedis <- renderPlotly({
    tp <- data %>%
      group_by(., Rating) 
    
    t <- plot_ly(tp, y = ~price, color = ~Rating, type = "box") 
    
    ggplotly(t) %>%
    layout( paper_bgcolor='white',
             plot_bgcolor = 'white',
             title = "Price Distribution",
            titlefont = list(family = "Arial",
                             size = 20),
            margin = list(b = '50px',
                          t = '50px'))
    
  })
  
  output$map <- renderPlotly({
    chmap2$hover <- with(chmap2,
                         paste(country,'<br>', "Classic",
                               Classic, '<br>', "Outstanding", Outstanding,
                               '<br>',"Very Good", Very.Good,'<br>',
                               "Good", Good))
    
    l <- list(color = toRGB("grey"), width = 0.5)
   
    ax <- list(
      title = "",
      zeroline = TRUE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      tickfont = list(size = 10)
    )
    
    g <- list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = "robinson"),
      paper_bgcolor='silver',
      plot_bgcolor = 'silver')
    
    m <- plot_geo(chmap2) %>%
      add_trace(
        z= ~Total, text = ~hover, color = ~Total, 
        colorscale = 'Jet', locations = ~CODE.x,
        marker = list(line = l)) %>%
      colorbar(title = 'Total Wines',
               titlefont = list(family = "Balto",
                                 size = '25px')) 
    
    ggplotly(m) %>%
      layout(
             geo = g,
             autosize = T,
             xaxis = ax,
             yaxis = ax,
             paper_bgcolor ='white',
             plot_bgcolor = 'white')
  })
  
  output$datat <- DT::renderDataTable({
    data$X <- NULL
    data$V1 <- NULL
    head(data, 100)
    
  })
  
  output$tab <- renderUI({
    url <- a("Kaggle.", 
                   href = "https://www.kaggle.com/zynicide/wine-reviews")
    tagList("This dataset is taken from",url)
  })
 
}


shinyApp(ui, server)

