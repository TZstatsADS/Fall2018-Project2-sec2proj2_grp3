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
       
                       tabPanel("Owner's Choice", icon = icon("map"),  fluidPage(
                         fluidRow(
                           tags$head(
                             # Include our custom CSS
                             includeCSS("../lib/styles.css"),
                             includeScript("../lib/click_hover.js")
                           ),
                           
                           # tags$div(id="searchBar",
                           #          column(width=1,
                           #                 style = "width:270px;display:inline-block;margin-right: 0px;margin-bottom:0px;margin-top:0px;padding-right:0px",
                           #                 textInput(inputId="location",label="", value="", placeholder = "search your location...")
                           #          ),
                           #          column(width=1,
                           #                 style = "margin-top: 25px;display:inline-block;margin-right: 0px;margin-left: 0px;left:0px;bottom:5px;padding-left:0px",
                           #                 actionButton("button1",label="", icon = icon("search"))
                           #                 
                                    # )),
                           column(3, h1("Characterize Your Restaurant")),
                           column(2, selectInput("check2_type", "Restaurant Type:", 
                                                 c(" " = "", list("American", "Quick Meal", "Asian", "Chinese", "Dessert", "European", "Italian", 
                                                                  "Mexican", "Seafood", "Others")), multiple=TRUE)),
                           column(3,
                                  sliderInput("check2_ppr", "People Per Restaurant:",min = 500, max = 10000, value = 500)),
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
                                  selectInput("check2_market","Market:", c(" " = "", list("1","2","3")))),
                           column(2, selectInput("check2_trans", "Transportation:", c(" " = "", list("1","2","3")))),
                           column(2, selectInput("check2_ct", "Cinema/Theater:", 
                                                 c(" " = "", list("1","2","3")))),
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
                                  fluidRow(column(1,actionButton("click_back_buttom",label="Click here back to original view")))    
                           )
                         ) 
                         )
                      )     
            )
  )
)

