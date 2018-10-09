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
                       
                       ######################################### I cook food #############################################                
                       tabPanel("All about map",
                                div(class="outer",
                                    tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                    leafletOutput("map", width = "120%", height = "120%"),
                                    absolutePanel(id = "controls", class = "panel panel-success", fixed = TRUE, draggable = FALSE,
                                                  top = 50, left = 0, height = 900,width = 320,
                                                  h2("Explore the area",align="center"),
                                                  h3("Click a Place on the Heatmap",align="center"),
                                                  hr(),
                                                  h4(textOutput("zip_text"),align="left"),
                                                  h4(textOutput("Total_population"),align="left"),
                                                  h4(textOutput("med_income_text"),align="left"),
                                                  h4(textOutput("Female.per"),align="left"),
                                                  h4(textOutput("yelp.rate"),align="left"),
                                                  
                                                  plotOutput("pie_female", width = "100%", height = "300px"),
                                                  plotOutput("pie_type", width = "100%", height = "300px")
                                                  
                                                  )))
                       
            )))
                                               
                                               
                                    
                                
                                
                                
                                      