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
           column(width=2,
                  selectInput("collection",
                              "Collection:",
                              c("BIOEENVIS","EVS_UMR5600","OSR","LEHNA","ECOMIC"))),
           column(width=2,
                  radioButtons("doctype",
                               "Type of document",
                               c("articles only","all documents"))),
           column(width=2,
                  sliderInput("years",
                              "years:",
                              min = 1970,
                              max = 2025,
                              value=c(2020,2025))),
           column(width=4,
                  "Exploration des collections HAL par les graphes de collaborations et les fréquences de mots")
           ),
  tabsetPanel(
    tabPanel(
      "Collaborations",

      # Sidebar with a slider input for number of bins
      fluidRow(
        column(width=2,
               selectInput("groups",
                           "collaboration between:",
                           c("people","labs")),
               selectInput("sizevar",
                           "Size nodes according to:",
                           choices=c("nrefs")),
               selectInput("namevar",
                           "Display labels corresponding to:",
                           choices=c("name","name_simplified","lemma"),
                           selected="name_simplified"),
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
                           value=50)
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
    tabPanel("Words",
             plotOutput("wordfreq")),
    tabPanel("Words by period",
             fluidRow(
               column(width=4,
                      sliderInput("cutyear","cut",min=2000,max=2025,value=2012,step=1)),
               column(width=3,
                      radioButtons("freq_or_spec","Scores based on",c("frequency","specificity"),selected="specificity")),
               column(width=3,
                      sliderInput("number_of_words","Number of words to display",min=5,max=100,value=20))
             ),
             plotOutput("word_period",height="800px")),
    tabPanel("References table",
             fluidRow(
                        column(width=2,
                               radioButtons("varsearch",
                                            "Search in:",
                                            c("title","keywords","abstract","text")),
                               HTML("Column <b>text</b> gathers title, keywords and abstract.")),
                        column(width=3,
                               textInput("textsearch",
                                         "Search for:",
                                         ""),
                               checkboxInput("show_abstract","Show abstract in table",value=FALSE)),
                        column(width=7,
                               plotOutput("plot_ref", height="300px"))
             ),
             DT::dataTableOutput("ref_authors"))
  )#tabsetPanel
)
