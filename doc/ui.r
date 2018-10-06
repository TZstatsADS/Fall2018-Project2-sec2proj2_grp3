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
  fluidPage(includeCSS("../lib/style.css"),
            navbarPage(p(class="h","All-N-Food"),id = "inTabset", 
                       
                       fluid=T,
                       tabPanel("I cook food",
                                div(
                                  class="outer",
                                  tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                  leafletOutput("map1", width = "120%", height = "120%"),
                                  absolutePanel(id = "controls", class = "panel panel-danger", fixed = TRUE, draggable = TRUE,
                                                top = 50, left = 0, height = "auto",width = 250,
                                                h3("Explore the opportunities",align="center"),
                                                hr(),
                                                h4(textOutput("zip_text"),align="left"),
                                                h4(textOutput("avgprice_text"),align="left")
                                                ,
                                                hr(),
                                                checkboxInput("click_multi","Show Your Trace", value = F),
                                                actionButton("click_reset_buttom","Reset")
                                  ))
                       ),
                       
                       tabPanel("I eat food", icon = icon("map"),  fluidPage(
                         fluidRow(
                           tags$head(
                             # Include our custom CSS
                             includeCSS("../lib/styles.css"),
                             includeScript("../lib/click_hover.js")
                             
                           ),
                           
                           tags$div(id="searchBar",
                                    column(width=1,
                                           style = "width:270px;display:inline-block;margin-right: 0px;margin-bottom:0px;margin-top:0px;padding-right:0px",
                                           textInput(inputId="location",label="", value="", placeholder = "search your location...")
                                    ),
                                    column(width=1,
                                           style = "margin-top: 25px;display:inline-block;margin-right: 0px;margin-left: 0px;left:0px;bottom:5px;padding-left:0px",
                                           actionButton("button1",label="", icon = icon("search"))
                                           
                                    )),
                           column(width=1, 
                                  style="margin-top: 25px;display:inline-block;margin-right: 10px",
                                  dropdownButton(circle = FALSE,
                                                 label = "type", status = "default", 
                                                 selectInput(inputId="min_bedrooms", label="choose", choices = c("American"=0,"Chinese"=1,"Japaness"=2,"Mexican"=3,"Middle-East"=4,"Korean"=5,"Latin-American"=6)
                                                             
                                                 ))
                           ),
                           
                           column(width=1,
                                  style = "margin-top: 25px;display:inline-block;margin-right: 0px;margin-left: 120px",
                                  dropdownButton(circle = FALSE,
                                                 label="Min price",  status = "default",
                                                 numericInput(inputId="min_price", label = "choose",value=0, min=0,max=1000000,step=1000)
                                  )
                                  
                           ),
                           column(width=1,
                                  style = "margin-top: 25px;display:inline-block;margin-right: 0px;",
                                  dropdownButton(circle = FALSE,
                                                 label="Max price",  status = "default", 
                                                 numericInput(inputId="max_price", value=1000000, label="choose",min=0,max=1000000,step=1000 )
                                  )),
                           column(width=1,
                                  style = "margin-top: 25px;;display:inline-block;margin-right: 10px;",
                                  dropdownButton(circle = FALSE,
                                                 label = "Rating", status = "default",
                                                 selectInput(inputId="min_bathrooms", label="choose", choices = c("Wrost"=0,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"Excellent"=6)
                                                             
                                                 ))),
                           
                           column(width=1, 
                                  style = "margin-top: 25px;display:inline-block;margin-right: 0px;",
                                  actionButton("button2",label="Reset" 
                                               
                                  ))),
                         
                         hr(),
                         
                         #                    mainPanel(
                         
                         fluidRow(
                           #column
                           
                           column(width=6, 
                                  # br(),
                                  # br(),
                                  # h3("current rank"),
                                  dataTableOutput("rank")
                           ),
                           
                           
                           
                           column(width=5,
                                  leafletOutput("map", width = "120%", height = 600)
                                  
                           )
                           
                         )
                         #)
                       )
                       
                       
                       )     
            )
  )
)

