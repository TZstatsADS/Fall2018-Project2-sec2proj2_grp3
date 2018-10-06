library(shiny)
library(leaflet)
library(data.table)
library(plotly)
library(DT)
shinyUI(
        fluidPage(includeCSS("style.css"),
                navbarPage(p(class="h","All-N-Food"),id = "inTabset", 
                              tabPanel("I cook food",
                                    div(class="outer",
                                        tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                        leafletOutput("map", width = "120%", height = "120%"),
                                        absolutePanel(id = "controls", class = "panel panel-danger", fixed = TRUE, draggable = TRUE,
                                                      top = 100, left = 10, height = "auto",width = 243,
                                                      hr(),
                                                      h3("It's your choice",align="center"),
                                                      hr(),
                                                      h4(textOutput("zip_text"),align="left"),
                                                      h4(textOutput("avgprice_text"),align="left")
                                                      ,
                                                      hr(),
  
                                                      checkboxInput("click_multi","Show Your Trace", value = F),
                                                      actionButton("click_reset_buttom","Reset to original view")
                                                       ))
                                    )

                )
        )
)

