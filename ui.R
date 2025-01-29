#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(HALtere)

# Define UI for application that draws a histogram
fluidPage(
  fluidRow(column(width=2,
                  tags$img(src = "hex-HALtere.png",
                           height = "50px", width = "50px")),
           column(width=3,
                  selectInput("collection",
                              "Collection:",
                              c("BIOEENVIS","EVS_UMR5600","OSR","LEHNA"))),
           column(width=7,
                  "Exploration des collections HAL par les graphes de collaborations et les fréquences de mots")),
  tabsetPanel(
    tabPanel(
      "Collaborations",

      # Sidebar with a slider input for number of bins
      fluidRow(
        column(width=2,
               selectInput("ind",
                           "collaboration between:",
                           c("people","labs")),
               sliderInput("years",
                           "years:",
                           min = 1970,
                           max = 2025,
                           value=c(2020,2025)),
               selectInput("sizevar",
                           "Size nodes according to:",
                           choices=c("nrefs")),
               selectInput("namevar",
                           "Display labels corresponding to:",
                           choices=c("name","affiliation","lemma")),
               selectInput("colorvar",
                           "Color nodes according to:",
                           choices=c("affiliation","name")),
               sliderInput("number_of_nodes",
                           "Number of nodes to display:",
                           min=0,
                           max=400,
                           value=400),
               sliderInput("number_of_names",
                           "Number of names to display:",
                           min=0,
                           max=50,
                           value=50),
               radioButtons("doctype",
                            "Type of document",
                            c("articles only","all documents"))
        ),

        # Show a plot of the generated distribution
        column(width=10,
               tabsetPanel(
                  tabPanel("graph",
                           plotly::plotlyOutput("collab_graph",
                                    width = "100%",
                                    height = "800px")),
                  tabPanel("nodes",
                           DT::dataTableOutput("table_nodes")),
                  tabPanel("links",
                           DT::dataTableOutput("table_edges"))
               )#tabsetPanel
        )
      )
    ),# tabPanel collaborations
    tabPanel("Mots",
             plotOutput("word_graph")
    )
  )#tabsetPanel
)
