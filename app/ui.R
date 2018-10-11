library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(htmltools)
library(shiny)
library(leaflet)
library(data.table)

shinyUI(
  fluidPage(includeCSS("www/style.css"),
            navbarPage(p(class="h","Open-a-Restaurant"),id = "inTabset", fluid=T,

                       tabPanel("Home",icon=icon("home"),
                                div(class="home",
                                    tags$head(
                                      # Include our custom CSS
                                      includeCSS("www/styles.css"),
                                      includeScript("www/click_hover.js")
                                    ),
                                    align="center"
                                )
                       ),
                       

                       tabPanel("Map-it!", icon = icon("map"),
                                div(class="outer",
                                    tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                    leafletOutput("map", width = "120%", height = "120%"),
                                    absolutePanel(id = "controls", class = "panel panel-success", fixed = TRUE, draggable = FALSE,
                                                  top = 50, left = 0, height = 900,width = 320,
                                                  h2("Explore the Area",align="center"),
                                                  h3("Click an area on the map",align="center"),
                                                  hr(),
                                                  h4(textOutput("zip_text"),align="left"),
                                                  h4(textOutput("Total_population"),align="left"),
                                                  h4(textOutput("med_income_text"),align="left"),
                                                  h4(textOutput("Female.per"),align="left"),
                                                  h4(textOutput("yelp.rate"),align="left"),
                                                  plotOutput("pie_age_rest", width = "100%", height = "300px")
                                    ))),
                      
       
                       tabPanel("Find-it", icon = icon("search"),  fluidPage(
                         fluidRow(
                           tags$head(
                             # Include our custom CSS
                             includeCSS("www/styles.css"),
                             includeScript("www/click_hover.js")
                           ),
                           
                           
                           column(3, h2("Optimize restaurant location")),
                           column(2, selectInput("check2_type", "Restaurant Type:", 
                                                 c(" " = "", list("American", "Quick Meal", "Asian", "Chinese", "Dessert", "European", "Italian", 
                                                                  "Mexican", "Seafood", "Others")), multiple=TRUE)),
                           column(3,
                                  sliderInput("check2_ppr", "People Per Restaurant:",min = 100, max = 10000, value = 500)),
                           column(2,
                                  selectInput("check2_class", "Residence Class:",c(" " = "", list("Upper", "Middle", "Working", "Lower")), 
                                              multiple=TRUE)),
                           column(2,
                                  selectInput("check2_age", "Age Targets :", c(" " = "", list("<5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")),
                                              multiple=TRUE))
                         ),
                         
                         fluidRow(
                           column(3,
                                  selectInput("check2_crime", "Crimes:", c("Acceptable number of crimes" = "", 
                                                                           list("Very Safe", "Safe", "A Little Dangerous", "Dangerous")))),
                           column(3,
                                  selectInput("check2_market","Market:", c(" " = "", list("Many", "A few","Don't care")))),
                           column(2, selectInput("check2_trans", "Transportation:", c(" " = "", list("Many","A few","Don't care")))),
                           column(2, selectInput("check2_ct", "Cinema/Theater:", c(" " = "", list("Many","A few","Don't care")))),
                           column(2, style = "margin-top: 25px;display:inline-block;margin-right: 0px;",
                                  div(id = "action",actionButton("button2", "Reset")))
                         ),
                         
                         hr(),
                         
                         fluidRow(
                           #column
                           
                           column(width=6, 
                                  dataTableOutput("table2")
                           ),
                           column(width=5,
                                  leafletOutput("map3", width = "120%", height = 480),
                                  fluidRow(column(1,actionButton("click_back_button",label="Click here back to original view")))    
                           )
                         ) 
                       )
                       )
                       # ,
                       # tabPanel('Contact information',icon=icon('home'),
                       #          div(class="outer"),
                       #          mainPanel(img(src='first_panel.png', height="170%", width="150%", align = "center")))
           
            )
  ))
